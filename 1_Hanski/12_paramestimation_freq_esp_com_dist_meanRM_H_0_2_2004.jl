# Code that integrates the Hanski model to obtain the solutions 
# With input: (from code input_Hanski.R)
#     . Flow matrix
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

# Input data for this model in /home/marta/albo_mobility/code/Hanski/ESP/input_Hanski_agg.R

# Choose location
loc = "/home/usuaris/m.pardo/"
print("Start code\n")
# loc = "/home/marta/"
#loc = "G:/mpardo/"

# Constant extract from https://www.nature.com/articles/s41598-017-12652-5
const m_c = 0.0051 # probability of mosquito in a car

# Load data input ---------------------------------------------------------------
const eta = Matrix(CSV.read(loc*"/albo_mobility/data/InputHanski/ESP/flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv",
DataFrame, header = true))[1:end,2:end]
const dist = Matrix(CSV.read(loc*"/albo_mobility/data/InputHanski/ESP/dist_mat_com_ESP.csv",
DataFrame, header = true))[1:end,2:end]
const N = size(eta, 1) # Number of patches
pop_init = zeros(N) # Initial conditions

# Load observations
obs = CSV.read(loc*"albo_mobility/data/InputHanski/ESP/pa_com.csv",DataFrame)
year_ic = 2005
Col_id = obs[(obs.year_detec .< year_ic),:]
Col_id = Col_id[(Col_id.year_detec .> 0),:].Column1

# Set p(0)_i = 1 for the places with aedes albopictus detections in 2006 
pop_init[Col_id] .= 1

# Load time series RM 
R_M = CSV.read(loc*"albo_mobility/data/InputHanski/ESP/3_rm_alb_ESP_com_0_2_v2.csv",DataFrame)

# Add row number as time
R_M.time = 1:nrow(R_M)

# Select time related to 15 august for initial condition
time_IC = R_M[R_M.date .== Date(string(year_ic)*"-08-15", "yyyy-mm-dd"),:].time
time_end = R_M.time[end]

# Function to compute the moving average to smooth a time series
# Input: 
#         .arr: vector with time series
#         .window_size: window size of the moving average
# Output: Smooth time series
function moving_average(arr, window_size)
  return [mean(arr[max(1, i - window_size + 1):i]) for i in 1:length(arr)] 
end 

# Function to do the moving average and interpolate a time series ---------------------
function interpolate_timeseries!(int_vec, df, window_size)
  df = sort(df, :date)
  # Select columns from the second to the second-to-last column, excluding the third column
  df = df[:, Not([1])]
  df = filter(row -> year(row.date) > 2004, df )

  # Apply moving average smoothing to each column except the first one (time column) 
  if window_size > 1
    for col in names(df)[2:end] 
      df[!, col] = moving_average(df[!, col], window_size)
    end
  end

  # Interpolate R_M
  dates = df[:, 1]
  dfs = Matrix(df[:, 2:end])
  dates_num = collect(1:size(dates, 1))

  # Perform interpolation for each location
  for i in 1:N
      # Extract temperature values for the current location
      dfs_val = dfs[:, i]

      # Perform linear interpolation
      itp = DataInterpolations.CubicSpline(dfs_val, dates_num, extrapolate = true)
      # Store the interpolated function
      int_vec[i] = itp
  end
    
end

# Interpolate R_M --------------------------------------------------------------------
# Create an array to store interpolated functions
InterpType = DataInterpolations.CubicSpline{Vector{Float64}, Vector{Int64},
 Vector{Int64}, Vector{Float64}, Float64}
interpolated_functions = Vector{InterpType}(undef, N)
interpolate_timeseries!(interpolated_functions, R_M, 7)

# # Test Interpolation
# new_x = collect(1:size(R_M[:, 2], 1))
# p = plot(new_x, interpolated_functions[318].(new_x),
#   label = "Cubic Spline")
# plot!(p,new_x, R_M[:,117], label = "Data")

# Interpolate Min temp --------------------------------------------------------------------
# Read RM data time series
tmin = CSV.read(loc*"albo_mobility/data/InputHanski/ESP/min_temp_ESP.csv",DataFrame)
# Interpolate RM and min temp. First create a global variable
interpolated_functions_mint = Vector{InterpType}(undef, N)
interpolate_timeseries!(interpolated_functions_mint, tmin, 1)
print("After interpolation") 

# Compute average tmin and RM
R_M_mean = vec(sum(Matrix(R_M[:,3:(end-1)]), dims = 1)/size(R_M,1))
tmin_mean = vec(sum(Matrix(tmin[:,3:(end)]), dims = 1)/size(tmin,1))

# Load yearly tmin
tmin_min = CSV.read(loc*"albo_mobility/data/InputHanski/ESP/min_temp_yearly_mean_ESP.csv",DataFrame)[:,2]

# # Test Interpolation
# using StatsPlots
# new_x = collect(1:size(tmin[:, 2], 1))
# p = plot(new_x, interpolated_functions_mint[1].(new_x),
#  ylim = (-10,10), label = "Cubic Spline")
# plot!(p,new_x, tmin[:,117], label = "Data")
# x_val = tmin.date
# y_val = tmin[:,5]
# plot(x_val, y_val, label = "Data")

# Non autnomous model ----------------------------------------------------------------------
function fun_na!(du, u, p, t)
  for i in 1:N
    du[i]= p[1]*R_M_mean[i] *sum(j -> exp(clamp(-(1/p[2])*dist[i,j],-700,700))*u[j], 1:N) * (1 - u[i]) -
    exp(clamp(p[3]* tmin_min[i] + p[4], -700,700)) * u[i]
  end

end

p=[0.001, 27861.720, -0.225, -6.45]
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

# Test model performance -------------------------------------------------------------------
# Pkg.add("BenchmarkTools")
# prob = DifferentialEquations.ODEProblem(fun_na!, u0, (0.0,1000.0), p)
# @time sol = solve(prob,alg_hints=[:stiff];p=p)
# plot(sol)
# @benchmark sol = solve(prob1,alg;p=$p) # Benchmark solution

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

# Bounds
low_bound = Float64[0,100,-2,-30]
up_bound = Float64[1,100000,0,0]

# Optimizatin -------------------------------------------------------------------------------
adtype = Optimization.AutoForwardDiff() #AutoFiniteDiff()
optf = Optimization.OptimizationFunction((p,x) -> summer_loss_by_year(p), adtype) 

# # Initial guess
# init_param = [0.1 ,0.050 ,9 ,-0.18,-10]
# # println("init_param:", init_param)

# # # Create the optimization problem with the new parameters
# optprob = Optimization.OptimizationProblem(optf, init_param, lb = low_bound, ub = up_bound) # Create the optimization problem

# DataFrame to store results
results_df = DataFrame(init_param1 = Float64[],init_param2 = Float64[],init_param3 = Float64[],init_param4 = Float64[],
                       opt_param1 = Float64[], opt_param2 = Float64[], opt_param3 = Float64[],opt_param4 = Float64[],
                       objective = Float64[])

# Print number of threads
println("num threads:",Threads.nthreads())
flush(stdout)

# Test one try
# optsol = Optimization.solve(optprob, BFGS(), maxiters = 1000)
# result_row = hcat(p..., optsol.u..., optsol.objective)

# Loop through different initial parameters parallely --------------------------------------------
# Threads.@threads 
for i in 1:20
  init_param = [0.0007187580359992243, 29247.648984832715, -0.08622800189475534, -8.467275973453438] #rand(3)*0.01
  # init_param[1] = (rand(1)[1]/1000)
  # init_param[2] = (rand(1)[1]*1000)
  # init_param[3] = (rand(1)[1]/10000)

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
    result_row = hcat(init_param..., zeros(3)..., 0.0)
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
filename = loc*"ESP_IC_2006_obs_2006-2023_com_dist_RM_2_"*current_date*".csv"
CSV.write(filename, results_df)

# Compute the solutions
p =[0.0005639646833044143, 32553.774005521933, -0.18821772990171218, -8.597835989480085]
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/output_estimation_dist_meanRM_tmin_H_0_2_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))
