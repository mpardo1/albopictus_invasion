# Code that integrates the Hanski model to obtain the solutions 
# With input: (from code albopictus_invasion/1_Hanski/11_input_Hanski_agg.R)
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

# Input data for this model in 1_Hanski/11_input_Hanski_agg.R
# Choose location
path_out = "data/output/"

# Constant extract from https://www.nature.com/articles/s41598-017-12652-5
const m_c = 0.0051 # probability of mosquito in a car

# Load data input ---------------------------------------------------------------
const eta = Matrix(CSV.read(path_out*"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv",
DataFrame, header = true))[1:end,2:end]
eta[diagind(eta)] .=0
const dist = Matrix(CSV.read(path_out*"dist_mat_com_ESP.csv",
DataFrame, header = true))[1:end,2:end]
const N = size(eta, 1) # Number of patches
pop_init = zeros(N) # Initial conditions

# Load observations
obs = CSV.read(path_out*"pa_com_2024.csv",DataFrame)
obs[obs.year_detec .>= 2023,:]
year_ic = 2023
Col_id = obs[(obs.year_detec .< year_ic),:]
Col_id = Col_id[(Col_id.year_detec .> 0),:].Column1

# Set p(0)_i = 1 for the places with aedes albopictus detections in 2006 
pop_init[Col_id] .= 1

# Load time series RM 
R_M = CSV.read(path_out*"rm_alb_ESP_com_future.csv",DataFrame)
R_M = R_M[year.(R_M.date) .< 2050,:]
R_M = R_M[:, Not([1])]
R_M = sort(R_M, :date)
R_M.time .= dates_num

# Compute average tmin and RM
R_M_mean = vec(sum(Matrix(R_M[:,3:(end-1)]), dims = 1)/size(R_M,1))
tmin_mean = vec(sum(Matrix(tmin[:,3:(end)]), dims = 1)/size(tmin,1))

# Load yearly tmin
tmin_min = CSV.read(path_out*"min_temp_yearly_mean_fut_ESP.csv",DataFrame)[:,3]

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
filename = path_out*"com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Scenarios human_mediated dispersal--------------------------------------------------------
p = [0.01*0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
# Compute solution
sol = hanski_prediction(p)
plot(sol)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = path_out*"low_sig_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
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
filename = path_out*"high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))

# Scenarios high extinction low human mediated--------------------------------------------------------
p = [0.01*0.0005706731568571676, 97.78894801161957, 5424.950376420903,
6.97762397002697e-5, 51314.7750145224, -6.874687389443576, -80.94731156667044]
p[6:7] = 0.05.*p[6:7]
# Compute solution
sol = hanski_prediction(p)
summer_avg_by_year = average_summer_solution_by_year(sol)
current_date = Dates.format(Dates.today(), "yyyy-mm-dd")
filename = path_out*"low_sig_high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_"*current_date*".csv"
CSV.write(filename, DataFrame(summer_avg_by_year, :auto))
