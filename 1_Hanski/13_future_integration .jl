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
 OptimizationPolyalgorithms, SciMLSensitivity, Zygote, Dates
# Input data for this model in /home/marta/albo_mobility/code/Hanski/ESP/input_Hanski_agg.R

# Choose location
loc = "data/output"
print("Start code\n")
# loc = "/home/marta/"
#loc = "G:/mpardo/"

# Constant extract from https://www.nature.com/articles/s41598-017-12652-5
const m_c = 0.0051 # probability of mosquito in a car

# Load data input ---------------------------------------------------------------
const eta = Matrix(CSV.read(loc*"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv",
DataFrame, header = true))[1:end,2:end]
eta[diagind(eta)] .=0
const dist = Matrix(CSV.read(loc*"dist_mat_com_ESP.csv",
DataFrame, header = true))[1:end,2:end]
const N = size(eta, 1) # Number of patches
pop_init = zeros(N) # Initial conditions

# Load observations
obs = CSV.read(loc*"pa_com_2024.csv",DataFrame)
obs[obs.year_detec .>= 2023,:]
year_ic = 2023
Col_id = obs[(obs.year_detec .< year_ic),:]
Col_id = Col_id[(Col_id.year_detec .> 0),:].Column1

# Set p(0)_i = 1 for the places with aedes albopictus detections in 2006 
pop_init[Col_id] .= 1

# Load time series RM 
R_M = CSV.read(loc*"rm_alb_ESP_com_future.csv",DataFrame)
R_M = R_M[year.(R_M.date) .< 2050,:]
R_M = R_M[:, Not([1])]
R_M = sort(R_M, :date)
# Function to do the moving average and interpolate a time series ---------------------
function interpolate_timeseries!(int_vec, df, window_size)
  df = sort(df, :date)
  # Select columns from the second to the second-to-last column, excluding the third column
  df = df[:, Not([1])]

  # Interpolate R_M
  dfs = Matrix(df[:, 2:end])

  # Initialize a day counter that starts at 1 for the first date
  dates_num = Int[]  # Empty array to store day count values
  previous_day = 1      # Starting point for day count

  # Loop through the dates and calculate day counts
  for i in 1:length(df.date)
      if i == 1
          push!(dates_num, 1)  # First date is day 1
      else
          # Calculate the difference in days from the previous date and add 15 for the 15th of the next month
          push!(dates_num, previous_day + Dates.value(df.date[i] - df.date[i-1]))
      end
      previous_day = dates_num[end]  # Update the previous day to the current day's count
  end

  # Perform interpolation for each location
  for i in 1:N
      # Extract temperature values for the current location
      dfs_val = dfs[:, i]

      # Perform linear interpolation
      itp = DataInterpolations.CubicSpline(dfs_val, dates_num, extrapolate = true)
      # Store the interpolated function
      int_vec[i] = itp

      # plot(dates_num,dfs_val)
  end
end

# Interpolate R_M --------------------------------------------------------------------
# Create an array to store interpolated functions
InterpType = DataInterpolations.CubicSpline{Vector{Float64},
 Vector{Int64},
 Vector{Int64}, Vector{Float64}, Float64}
interpolated_functions = Vector{InterpType}(undef, N)
interpolate_timeseries!(interpolated_functions, R_M, 1)

# Compute dates num time
dates_num = Int[]  # Empty array to store day count values
previous_day = 1      # Starting point for day count

# Loop through the dates and calculate day counts
for i in 1:length(R_M.date)
    if i == 1
        push!(dates_num, 1)  # First date is day 1
    else
        # Calculate the difference in days from the previous date and add 15 for the 15th of the next month
        push!(dates_num, previous_day +
          Dates.value(R_M.date[i] - R_M.date[i-1]))
    end
    previous_day = dates_num[end]  # Update the previous day to the current day's count
end

# Test Interpolation
using StatsPlots
new_x = dates_num # Extract this from function interpolation
p = plot(R_M.date, R_M[:,120], label = "Data")
plot!(p,R_M.date, interpolated_functions[120].(new_x))
plot(new_x, interpolated_functions[10].(new_x))

# Interpolate Min temp --------------------------------------------------------------------
# Read RM data time series
tmin = CSV.read(loc*"min_temp_ESP_future.csv",DataFrame)
tmin = tmin[year.(tmin.date) .< 2050,:]
tmin = tmin[:, Not([1])]
tmin = sort(tmin, :date)
# Interpolate RM and min temp. First create a global variable
interpolated_functions_mint = Vector{InterpType}(undef, N)
interpolate_timeseries!(interpolated_functions_mint, tmin, 1)
print("After interpolation") 

# Test Interpolation
new_x = dates_num # Extract this from function interpolation
p = plot(tmin.date, tmin[:,120], label = "Data")
plot!(p,tmin.date, interpolated_functions_mint[120].(new_x))
plot(dates_num, interpolated_functions_mint[120].(new_x))

R_M.time .= dates_num

# Compute average tmin and RM
R_M_mean = vec(sum(Matrix(R_M[:,3:(end-1)]), dims = 1)/size(R_M,1))
tmin_mean = vec(sum(Matrix(tmin[:,3:(end)]), dims = 1)/size(tmin,1))

# Load yearly tmin
tmin_min = CSV.read(loc*"min_temp_yearly_mean_fut_ESP.csv",DataFrame)[:,3]

# Non autnomous model ----------------------------------------------------------------------
function fun_na!(du, u, p, t)
    for i in 1:N
        du[i]= R_M_mean[i]*sum(j -> ((p[1] / (1 + exp(clamp(-p[2] * m_c * eta[i,j] + p[3], -700,700))))+ 
        p[4]*exp(clamp(-(1/p[5])*dist[i,j],-700,700)))*u[j], 1:N) * (1 - u[i]) -
        exp(clamp(p[6]* tmin_min[i] + p[7], -700,700)) * u[i]
    end
  end

# Test speed
# using BenchmarkTools
# p = [0.1,0.0001,5, -0.5,-5] 
# @benchmark fun_na!($Vector{Float64}(undef,N), $pop_init, $p, 1)

# Set initial parameters ------------------------------------------------------------------
t0= dates_num[1] 
tf= dates_num[end] - 100
tspan = (t0, tf)
t_vect=1:tf
u0 = pop_init
p = [0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
p[6:7] = 0.05.*p[6:7]
# Create the model
prob = DifferentialEquations.ODEProblem(fun_na!, u0, tspan, p)
sol = solve(prob, alg=TRBDF2(); abstol=1e-8, reltol=1e-8)
plot(sol)

# Test
println("Last year R_M:", R_M.date[end])

# Create the cost function -----------------------------------------------------------------
vec_year = unique(year.(R_M.date))[1:end] # Vector of summer times per year

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

# Actual solution -------------------------------------------------
p = [0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
sol = hanski_prediction(p)
plot(sol)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Scenarios human_mediated dispersal--------------------------------------------------------
p = [0.01*0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
# Compute solution
sol = hanski_prediction(p)
plot(sol)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "low_sig_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Scenarios high extinction --------------------------------------------------------
p = [0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
p[6:7] = 0.05.*p[6:7]

# Compute solution
sol = hanski_prediction(p)
plot(sol)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Scenarios high extinction low human mediated--------------------------------------------------------
p = [0.01*0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
p[6:7] = 0.05.*p[6:7]
# Compute solution
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = "low_sig_high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))
