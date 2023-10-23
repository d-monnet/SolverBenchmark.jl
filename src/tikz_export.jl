export export_performance_profile_tikz
"""

function export_performance_profile_tikz(stats, costs, costnames, filename; kwargs...)

"""
function export_performance_profile_tikz(stats::Dict{Symbol, DataFrame},
  costs::Vector{<:Function},
  costnames::Vector{String},
  filename::String;
  xlim::AbstractFloat=10,
  ylim::AbstractFloat=10,
  nxgrad::Int=5,
  nygrad::Int=5,
  markers::Vector{String} = [],
  colours::Vector{String} = [],
  linestyles::Vector{String} = [],
  linewidth::Vector{String} = 1.0,
  xlabels::String = [],
  ylabels::String = [],
  kwargs...)

  logscale = true
  if haskey(kwargs,:logscale)
    logscale = kwargs[:logscale]
  end
  solvers = collect(keys(stats))
  solver_names = String.(keys(stats))
  scale = 1.0

  axis_tik_l = 0.2
  lgd_offset = 0.5
  lgd_box_length = 3.
  lgd_plot_length = 0.7
  lgd_v_offset = 0.7

  label_val = [0.2,0.25,0.5,1] # possible graduation labels along axes are multiples of label_val elements times 10^n
  ymax = 1.0
  y_grad = collect(0.:ymax/(nygrad-1):ymax)

  isempty(colours) && (colours = ["black" for _ in eachindex(solvers)])
  isempty(linestyles) && (linestyles = ["solid" for _ in eachindex(solvers)])
  #isempty(markers) && ()
  
  x_mat, y_mat = SolverBenchmark.get_profile_solvers_data(stats,costs;kwargs...)
  for i in eachindex(costnames)
    xlabel = isempty(xlabels) ? "Cost: $(costnames[i])" : xlabels[i]
    ylabel = isempty(ylabels) ? "\\% of problem solved" : ylabels[i]
    xel = x_mat[i]
    xmax , _ = findmax(xel[.!isnan.(xel)])
    dist = xmax/(nxgrad-1)
    n=log.(10,dist./label_val)
    _, ind = findmin(abs.(n .- round.(n)))
    xgrad_dist = label_val[ind]*10^round(n[ind])
    x_grad = [0. , [xgrad_dist*i for i =1 : nxgrad-1]...]
    if isinteger(x_grad[1])
      x_grad = Int.(x_grad)
    end
    x_grad[end] <= xmax || (pop!(x_grad))
    xratio = xlim*0.9/xmax
    yratio = ylim*0.9/ymax
    open(filename*"_$(costnames[i]).tex", "w") do io
      println(io, "\\begin{tikzpicture}")
      # axes
      println(io, "\\draw[->,line width=$linewidth] (0,0) -- ($xlim,0);")
      println(io, "\\node at ($(xlim/2), -1) {$xlabel};")
      println(io, "\\draw[->,line width=$linewidth] (0,0) -- (0,$ylim);")
      println(io, "\\node at (-1,$(ylim/2)) [rotate = 90]  {$ylabel};")
      # axes graduations and labels
      if logscale
        for i in eachindex(x_grad)
          println(io, "\\draw[line width=$linewidth] ($(x_grad[i]*xratio),0) -- ($(x_grad[i]*xratio),$axis_tik_l) node [pos=0, below] {\$2^{$(x_grad[i])}\$};")
        end
      else
        for i in eachindex(x_grad)
          println(io, "\\draw[line width=$linewidth] ($(x_grad[i]*xratio),0) -- ($(x_grad[i]*xratio),$axis_tik_l) node [pos=0, below] {$(x_grad[i])};")
        end
      end
      for i in eachindex(y_grad)
        println(io, "\\draw[line width=$linewidth] (0,$(y_grad[i]*yratio)) -- ($axis_tik_l,$(y_grad[i]*yratio)) node [pos=0, left] {$(y_grad[i])};")
      end

      # profiles
      for j in eachindex(solvers) 
        drawcmd = "\\draw[line width=$linewidth, $(colours[j]), $(linestyles[j]), line width = $linewidth] "
        drawcmd *= "($(x_mat[i][1,j]*xratio),$(y_mat[i][1,j]*yratio))"
        for k in 2:size(x_mat[i],1)
          if isnan(x_mat[i][k,j])
            break
          end
          if y_mat[i][k,j] > 1 # for some reasons last point of profile is set with y=1.1 ...
            drawcmd *= " -- ($(xmax*xratio),$(y_mat[i][k-1,j]*yratio)) -- ($(xmax*xratio),$(y_mat[i][k-1,j]*yratio))"
          else
            drawcmd *= " -- ($(x_mat[i][k,j]*xratio),$(y_mat[i][k-1,j]*yratio)) -- ($(x_mat[i][k,j]*xratio),$(y_mat[i][k,j]*yratio))"
          end
        end
        drawcmd *= ";"
        println(io,drawcmd)
      end
      # legend
      for j in eachindex(solvers) 
        legcmd = "\\draw[$(colours[j]), $(linestyles[j]), line width = $linewidth] "
        legcmd *= "($(xlim+lgd_offset),$(ylim-j*lgd_v_offset)) -- ($(xlim+lgd_offset+lgd_plot_length),$(ylim-j*lgd_v_offset)) node [pos=1,right] {$(String(solver_names[j]))}"
        legcmd *= ";"
        println(io,legcmd)
      end
      # legend box
      println(io,"\\draw[line width=$linewidth] ($(xlim+lgd_offset-0.1),$ylim) -- ($(xlim+lgd_offset+lgd_box_length),$ylim) -- ($(xlim+lgd_offset+lgd_box_length),$(ylim-lgd_v_offset*(length(solvers)+1))) -- ($(xlim+lgd_offset-0.1),$(ylim-lgd_v_offset*(length(solvers)+1))) -- cycle;")
      println(io,"\\end{tikzpicture}")
    end
  end
end