# Code that performs the parameter estimation for the Human-mediated model
# With input: (from code albopictus_invasion/1_Hanski/11_input_Hanski_agg.R)
#     . Flow matrix
#     . Distance matrix
#     . R_M time series
#     . t_min time series
# And estimate the parameters of the model from the presence absence data
# Load pkgs -----------------------------------------------------
import Pkg
# Pkg.add("OrdinaryDiffEq")
# Pkg.add("SciMLSensitivity")
# Pkg.add("Zygote")

using DifferentialEquations,  DataFrames,  CSV, Plots, LinearAlgebra, ODE, DataInterpolations,
 DiffEqParamEstim, Optimization,  Statistics, Dates,ForwardDiff, OptimizationOptimJL, OptimizationBBO, OrdinaryDiffEq,
 OptimizationPolyalgorithms, SciMLSensitivity, Zygote

# Choose location
path_out = "data/output/"

# Constant extract from https://www.nature.com/articles/s41598-017-12652-5
const m_c = 0.0051 # probability of mosquito in a car

# Load data input ---------------------------------------------------------------
const eta = Matrix(CSV.read(path_out*"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv",
DataFrame, header = true))[1:end,2:end]
const N = size(eta, 1) # Number of patches
pop_init = zeros(N) # Initial conditions

# Load observations
obs = CSV.read(path_out*"pa_com.csv",DataFrame)
year_ic = 2007
Col_id = obs[(obs.year_detec .< year_ic),:]
Col_id = Col_id[(Col_id.year_detec .> 0),:].Column1

# Set p(0)_i = 1 for the places with aedes albopictus detections in 2006 
pop_init[Col_id] .= 1

# Load time series RM 
R_M = CSV.read(path_out*"3_rm_alb_ESP_com_0_2_v2.csv",DataFrame)

# Add row number as time
R_M.time = 1:nrow(R_M)

# Select time related to 15 august for initial condition
time_IC = R_M[R_M.date .== Date(string(year_ic)*"-08-15", "yyyy-mm-dd"),:].time
time_end = R_M.time[end]

# Compute average tmin and RM
R_M_mean = vec(sum(Matrix(R_M[:,2:(end-1)]), dims = 1)/size(R_M,1))

# Load yearly tmin
tmin_min = CSV.read(path_out*"min_temp_yearly_mean_ESP.csv",DataFrame)[:,2]


# Non autnomous model ----------------------------------------------------------------------
function fun_na!(du, u, p, t)
  for i in 1:N
      du[i]= R_M_mean[i]*sum(j -> (p[1] / (1 + exp(clamp(-p[2] * m_c * eta[i,j] + p[3], -700,700))))*u[j], 1:N) * (1 - u[i]) -
              exp(clamp(p[4]* tmin_min[i] + p[5], -700,700)) * u[i]
  end
  # println("du:$du[1:5]")
end

# Test speed
# using BenchmarkTools
# p = [0.1,0.0001,5, -0.5,-5] 
# @benchmark fun_na!($Vector{Float64}(undef,N), $pop_init, $p, 1)
p = [0.01, 0.204, 6.87, -0.02, -10.42]

# Create matrix observations -------------------------------------------------------------
# Remove first column
obs = obs[:, Not([1])]

# Transform obs to a matrix wich row id_mitma and columns year, 1 if it was detected this year
# And the years after detection 0 otherwise
min_year = 2004
max_year = 2024

# Initialize an empty sparse matrix
num_ids = length(unique(obs.CO_COMARCA))
num_years = max_year - min_year + 1
matrix_obs = zeros(Float64,num_ids, num_years)

# Loop through unique id_mitma values
for (i, id) in enumerate(unique(obs.CO_COMARCA))
    # Get years for the current id_mitma
    years = obs.year_detec[obs.CO_COMARCA .== id]
    # Convert years to indices in the matrix
    if (years[1]>0  && years[1]<2024)
      ind1 =  years[1] - min_year + 1
      # Set entries to 1
      matrix_obs[i, ind1:num_years] .= 1
    end
end

# Remove first year, its the IC for the ODE
ind_ic = year_ic - 2004 + 1
matrix_obs = matrix_obs[:,3:end] # Years 2007-2023 included

# Create the vector of times of the observation. Assuming we observe in August 15 (or 16 depending onn leap years)
R_M.time = 1:nrow(R_M)
t_obs = filter(row -> month(row.date) == 8 && day(row.date) == 15, R_M)[:,[1,end]].time
# Remove years before ic
#t_obs = t_obs[(ind_ic-2):end]

# Set initial parameters ------------------------------------------------------------------
t0= t_obs[1] - 100
tf= t_obs[end] + 10
tspan = (t0, tf)
t_vect=1:tf
u0 = pop_init
p = [0.0004,0.15,10,0.1,28000,-0.22,-5.9]

# Create the model
prob = DifferentialEquations.ODEProblem(fun_na!, u0, tspan, p)

# Test
println("Last year R_M:", R_M.date[end])
# Create the cost function -----------------------------------------------------------------
vec_year = unique(year.(R_M.date))#[3:end] # Vector of summer times per year

# Function to extract times related to summer per year
function summer_times_by_year(R_M)
    summer_times = Dict{Int, Vector{Int}}()
    for y in vec_year
        summer_times[y] = filter(row -> year(row.date) == y && month(row.date) in 6:8, R_M).time
    end
    return summer_times
end

# Run through RM
summer_t_obs_by_year = summer_times_by_year(R_M)

# Function to compute average probability of occupancy in summer per year and comarca
function average_summer_solution_by_year(sol)    
  stack([[mean(sol(summer_t_obs_by_year[years], idxs = i)) for i in 1:N] for years in vec_year])
end

# Update p in the ODE
function hanski_prediction(p)
	_prob = remake(prob, p = p)
	sol = solve(_prob, alg=TRBDF2(); abstol=1e-8, reltol=1e-8)
  return(sol)
end

# @time hanski_prediction(p)

# Loss function
function summer_loss_by_year(p)
  println("p:", ForwardDiff.value.(p))
  sol = hanski_prediction(p)
  if any((!SciMLBase.successful_retcode(s.retcode) for s in sol)) # Test for not succesfull integration
      print("Loss function INF\n")
      flush(stdout)
      return Inf
  else
    summer_avg_by_year = average_summer_solution_by_year(sol)
    loss = sum((summer_avg_by_year .- matrix_obs).^2) 
    println("loss:", ForwardDiff.value.(loss))
    flush(stdout)
    return loss
  end
end

# Parameters bounds
low_bound = Float64[0,0,0,-1000,-6000]
up_bound = Float64[1,1,1000,0,0]

# Optimizatin -------------------------------------------------------------------------------
adtype = Optimization.AutoForwardDiff() #AutoFiniteDiff()
optf = Optimization.OptimizationFunction((p,x) -> summer_loss_by_year(p), adtype) 

# DataFrame to store results
results_df = DataFrame(init_param1 = Float64[], init_param2 = Float64[], init_param3 = Float64[],init_param4 = Float64[],init_param5 = Float64[],
                       opt_param1 = Float64[], opt_param2 = Float64[],  opt_param3 = Float64[], opt_param4 = Float64[], opt_param5 = Float64[],
                       objective = Float64[])

# Print number of threads
println("num threads:",Threads.nthreads())
flush(stdout)

# Loop through different initial parameters parallely --------------------------------------------
for i in 1:20

  init_param = [rand(1), rand(1), rand(1), -rand(1) -rand(1)]
  println("init_param:", init_param)
  result_row = []
  flush(stdout)

  # Create the optimization problem with the new parameters
  optprob = Optimization.OptimizationProblem(optf, init_param, lb = low_bound, ub = up_bound) # Create the optimization problem

  # Do a try catch to not break when integration problem
  try
    # @time optsol = Optimization.solve(optprob, BFGS(), maxiters = 2)
    optsol = Optimization.solve(optprob,OptimizationOptimJL.LBFGS(), maxiters = 1000)
    result_row = hcat(init_param..., optsol.u..., optsol.objective)
    println("result_row: \n $result_row")
    flush(stdout)
  catch e
    println("ERROR: $e")
    result_row = hcat(init_param..., zeros(5)..., 0.0)
    println("result_row: \n $result_row")
    flush(stdout)
  end
  
  # Ensure thread-safe access to results_df
  push!(results_df, result_row)
  flush(stdout)

end

# Save results in a csv and filter erros -------------------------------------------------------
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
# Create the filename with the date 
filename = path_out*"ESP_IC_2006_obs_2006-2023_com_sigmoid_tmin_"*current_date*".csv"
CSV.write(filename, results_df)

# Run simulation with estimated parameters
p=[0.00019500768571673895, 0.1707553296090941, 6.355391841965978, -0.7581545344650137, -44.14554324330141]
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
summer_avg_by_year = insertcols!(DataFrame(summer_avg_by_year, :auto), 1, :CO_COMARCA=> pa_com.CO_COMARCA) # Add CO_COMARCA ID
names(summer_avg_by_year)[2:end] .= string.(2005:2023) # Add years as name columns
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = path_out*"output_estimation_hummob_meanrm__IC_2004_tmin_H_0_2_"*current_date*".csv"
CSV.write(filename, summer_avg_by_year)

