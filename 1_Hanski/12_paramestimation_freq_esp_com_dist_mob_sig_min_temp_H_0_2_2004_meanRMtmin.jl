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
eta[diagind(eta)] .=0
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

# Load tmin data
tmin = CSV.read(loc*"albo_mobility/data/InputHanski/ESP/min_temp_ESP.csv",DataFrame)

# Compute average tmin and RM
R_M_mean = vec(sum(Matrix(R_M[:,3:(end-1)]), dims = 1)/size(R_M,1))
tmin_mean = vec(sum(Matrix(tmin[:,3:(end)]), dims = 1)/size(tmin,1))

# Load yearly tmin
tmin_min = CSV.read(loc*"albo_mobility/data/InputHanski/ESP/min_temp_yearly_mean_ESP.csv",DataFrame)[:,2]

# Non autnomous model ----------------------------------------------------------------------
function fun_na!(du, u, p, t)
  for i in 1:N
      du[i]= R_M_mean[i]*sum(j -> ((p[1] / (1 + exp(clamp(-p[2] * m_c * eta[i,j] + p[3], -700,700))))+ 
      p[4]*exp(clamp(-(1/p[5])*dist[i,j],-700,700)))*u[j], 1:N) * (1 - u[i]) -
      exp(clamp(p[6]* tmin_min[i] + p[7], -700,700)) * u[i]
  end
end

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
p= [0, 97.78894801161957, 5424.950376420903,
 6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]

# Test model performance -------------------------------------------------------------------
# Pkg.add("BenchmarkTools")
prob = DifferentialEquations.ODEProblem(fun_na!, u0, (0.0,365), p)
sol = solve(prob, alg=TRBDF2(); abstol=1e-8, reltol=1e-8,p=p)
plot(sol)
sol.u[end]
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/natural_dispersal_one_year_"*current_date*".csv"
CSV.write(filename, DataFrame(CO_COMARCA=pa_com.CO_COMARCA,out= sol.u[end]))
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

# Save matrix obs
filename = loc*"obs_2005-2023.csv"
CSV.write(filename, DataFrame(matrix_obs, :auto))

# Parameters bounds
low_bound = Float64[0,0,0,0,100,-30,-200]
up_bound = Float64[1,300,9000,1,100000,0,0]

# Optimizatin -------------------------------------------------------------------------------
adtype = Optimization.AutoForwardDiff() #AutoFiniteDiff()
optf = Optimization.OptimizationFunction((p,x) -> summer_loss_by_year(p), adtype) 

# DataFrame to store results
results_df = DataFrame(init_param1 = Float64[], init_param2 = Float64[], init_param3 = Float64[],
                        init_param4 = Float64[],init_param5 = Float64[],init_param6 = Float64[],init_param7 = Float64[],
                       opt_param1 = Float64[], opt_param2 = Float64[],  opt_param3 = Float64[],
                        opt_param4 = Float64[],opt_param5 = Float64[],opt_param6 = Float64[],opt_param7 = Float64[],
                       objective = Float64[])

# Print number of threads
println("num threads:",Threads.nthreads())
flush(stdout)

# Loop through different initial parameters parallely --------------------------------------------
for i in 1:20
  init_param = [0.0006334500051789295, 119.99999996168938, 6563.224727595308, 4.930360850946188e-5, 56728.50163105225, -1.5151044038490378, -39.24983327423533]
  init_param = [0.0005713395226173093, 97.98127997695242, 5432.979211108907, 6.959799465101372e-5, 51322.689445742195, -8.666629498231375, -99.94539382377171]
  # init_param[1] = init_param[1] + (rand(1)[1]/10)
  # init_param[2] = init_param[2] + (rand(1)[1]/1000)
  # init_param[3] = init_param[3] + (rand(1)[1]/100)
  # init_param[4] = init_param[4] + (rand(1)[1])
  # init_param[5] = init_param[5] + (rand(1)[1]*1000)
  # init_param[6] = init_param[6] - (rand(1)[1]/100)
  # init_param[7] = init_param[7] - (rand(1)[1])

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
    result_row = hcat(init_param..., zeros(7)..., 0.0)
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
filename = loc*"obs_2004_sigmob_dist_RM_tmin_avg_"*current_date*".csv"
CSV.write(filename, results_df)

# Estimated parameters
p= [0.0005706731568571676, 97.78894801161957, 5424.950376420903,
 6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/output_mean_tminRM_H_0_2_IC_2004_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Run knock out escenarios ------------------------------------------------------------------------------------
# No human mobility
p= [0, 97.78894801161957, 5424.950376420903, 6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/output_mean_tminRM_H_0_2_nohum_IC_2004_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# No natural dispersal
p= [0.0005706731568571676, 97.78894801161957, 5424.950376420903, 0, 51314.7750145224, -6.874687389443576, -80.94731156667044]
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/output_mean_tminRM_H_0_2_nodist_IC_2004_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Run with estimated parameters for one more year --------------------------------------------------------------
# Add 2024 as 2023
df_2023 = filter(row -> year(row.date) == 2023, R_M)
df_2024 = deepcopy(df_2023)
df_2024.date .= df_2024.date .+ Year(1)  # Shift dates to 2024
max_time = maximum(R_M.time)
df_2024.time .= (max_time + 1):(max_time + nrow(df_2024))
R_M = vcat(R_M, df_2024)
interpolated_functions = Vector{InterpType}(undef, N)
interpolate_timeseries!(interpolated_functions, R_M, 7)

# Read RM data time series
df_2023 = filter(row -> year(row.date) == 2023, tmin)
df_2024 = deepcopy(df_2023)
df_2024.date .= df_2024.date .+ Year(1)  # Shift dates to 2024
max_time = maximum(tmin.Column1)
df_2024.Column1 .= (max_time + 1):(max_time + nrow(df_2024))
tmin = vcat(tmin, df_2024)
interpolated_functions_mint = Vector{InterpType}(undef, N)
interpolate_timeseries!(interpolated_functions_mint, tmin, 1)
print("After interpolation") 

# Change time
t0=t_obs[end]
tf= t_obs[end] + 400
tspan = (t0, tf)
t_vect=1:tf
prob = DifferentialEquations.ODEProblem(fun_na!, u0, tspan, p)
p=[0.00034758402240939553, 0.35206796353051073, 19.96722933911206, 7.905021846152038e-5, 51978.61421064834, -1.3661892831684044, -25.681553759112123]
sol = solve(prob, alg=TRBDF2(); abstol=1e-8, reltol=1e-8)
vec_year = unique(year.(R_M.date))[3:end] # Vector of summer times per year
summer_t_obs_by_year = summer_times_by_year(R_M)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/com_opt_simulation_dits_sig_mob_tmin_RM_matrix_2024_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))


# Change time
t0=t_obs[end]
tf= t_obs[end] + 400
tspan = (t0, tf)
t_vect=1:tf
prob = DifferentialEquations.ODEProblem(fun_na!, u0, tspan, p)
p=[0., 0.35206796353051073, 19.96722933911206, 7.905021846152038e-5, 51978.61421064834, -1.3661892831684044, -25.681553759112123]
sol = solve(prob, alg=TRBDF2(); abstol=1e-8, reltol=1e-8)
vec_year = unique(year.(R_M.date))[3:end] # Vector of summer times per year
summer_t_obs_by_year = summer_times_by_year(R_M)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "/home/marta/Documentos/PHD/2024/Colonization/output/com_opt_simulation_dits_sig_no_mob_tmin_RM_matrix_2024_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))
