#ggbuild <- g
#filename <- "C:/Users/df/Desktop/Test2.tex"
#width <- "6"
#height <- "4"
#legendLocation <- "SE"
#xlabNotation <- "norm"
#ylabNotation <- "norm"
#legendMat <- matrix(c("GeneralID", "1"), nrow = 1, ncol = 2)

#scatter2pgf(ggbuild = g, filename = "C:/Users/df/Desktop/Test2.tex",
#            width = "6", height = "4", legendLocation = "SE",
#            xlabNotation = "norm", ylabNotation = "norm",
#            legendMat = matrix(c("GeneralID", "1"), nrow = 1, ncol = 2) )

scatter2pgf <- function(ggbuild,
                        filename,
                        width,
                        height,
                        xlabNotation,
                        ylabNotation,
                        subfigLab,
                        subFigPunc,
                        legendLocation,
                        legendMat){

  ## Error-checking
  if(!(legendLocation %in% c("NE", "SE", "SW", "NW"))){
    stop("Must specify a legend position: 'NE', 'SE', 'SW', 'NW'")
  }
  if(!(xlabNotation %in% c("norm", "exp"))){
    stop("Must specify an axis label notation: 'norm', 'exp'")
  }
  if(!(ylabNotation %in% c("norm", "exp"))){
    stop("Must specify an axis label notation: 'norm', 'exp'")
  }

  ## R point marker conversion to pgfplots
  markerTable <- data.table(R=c(112:127, 96:111, 80:95, 64:79, 48:63, 32:47, 16:25, 0:15),
                            pgf=c("text, text mark=p, mark options={solid}",
                                  "text, text mark=q, mark options={solid}",
                                  "text, text mark=r, mark options={solid}",
                                  "text, text mark=s, mark options={solid}",
                                  "text, text mark=t, mark options={solid}",
                                  "text, text mark=u, mark options={solid}",
                                  "text, text mark=v, mark options={solid}",
                                  "text, text mark=w, mark options={solid}",
                                  "text, text mark=x, mark options={solid}",
                                  "text, text mark=y, mark options={solid}",
                                  "text, text mark=z, mark options={solid}",
                                  "|, mark options={solid, rotate=135}",
                                  "Mercedes star, mark options={solid, rotate=90}",
                                  "Mercedes star, mark options={solid, rotate=270}",
                                  "triangle, mark options={solid, rotate=270}",
                                  "triangle*, mark options={solid, rotate=270}",
                                  "|, mark options={solid, rotate=90}",
                                  "text, text mark=a, mark options={solid}",
                                  "text, text mark=b, mark options={solid}",
                                  "text, text mark=c, mark options={solid}",
                                  "text, text mark=d, mark options={solid}",
                                  "text, text mark=e, mark options={solid}",
                                  "text, text mark=f, mark options={solid}",
                                  "text, text mark=g, mark options={solid}",
                                  "text, text mark=h, mark options={solid}",
                                  "text, text mark=i, mark options={solid}",
                                  "text, text mark=j, mark options={solid}",
                                  "text, text mark=k, mark options={solid}",
                                  "text, text mark=l, mark options={solid}",
                                  "text, text mark=m, mark options={solid}",
                                  "text, text mark=n, mark options={solid}",
                                  "text, text mark=o, mark options={solid}",
                                  "text, text mark=P, mark options={solid}",
                                  "text, text mark=Q, mark options={solid}",
                                  "text, text mark=R, mark options={solid}",
                                  "text, text mark=S, mark options={solid}",
                                  "text, text mark=T, mark options={solid}",
                                  "text, text mark=U, mark options={solid}",
                                  "text, text mark=V, mark options={solid}",
                                  "text, text mark=W, mark options={solid}",
                                  "text, text mark=X, mark options={solid}",
                                  "text, text mark=Y, mark options={solid}",
                                  "text, text mark=Z, mark options={solid}",
                                  "|, mark options={solid, rotate=45}",
                                  "triangle, mark options={solid, rotate=90}",
                                  "triangle*, mark options={solid, rotate=90}",
                                  "halfdiamond*, mark options={solid, rotate=225}",
                                  "halfdiamond*, mark options={solid, rotate=315}",
                                  "|, mark options={solid}",
                                  "text, text mark=A, mark options={solid}",
                                  "text, text mark=B, mark options={solid}",
                                  "text, text mark=C, mark options={solid}",
                                  "text, text mark=D, mark options={solid}",
                                  "text, text mark=E, mark options={solid}",
                                  "text, text mark=F, mark options={solid}",
                                  "text, text mark=G, mark options={solid}",
                                  "text, text mark=H, mark options={solid}",
                                  "text, text mark=I, mark options={solid}",
                                  "text, text mark=J, mark options={solid}",
                                  "text, text mark=K, mark options={solid}",
                                  "text, text mark=L, mark options={solid}",
                                  "text, text mark=M, mark options={solid}",
                                  "text, text mark=N, mark options={solid}",
                                  "text, text mark=O, mark options={solid}",
                                  "text, text mark=0, mark options={solid}",
                                  "text, text mark=1, mark options={solid}",
                                  "text, text mark=2, mark options={solid}",
                                  "text, text mark=3, mark options={solid}",
                                  "text, text mark=4, mark options={solid}",
                                  "text, text mark=5, mark options={solid}",
                                  "text, text mark=6, mark options={solid}",
                                  "text, text mark=7, mark options={solid}",
                                  "text, text mark=8, mark options={solid}",
                                  "text, text mark=9, mark options={solid}",
                                  "halfdiamond*, mark options={solid}",
                                  "halfdiamond*, mark options={solid, rotate=90}",
                                  "halfdiamond*, mark options={solid, rotate=180}",
                                  "halfdiamond*, mark options={solid, rotate=270}",
                                  "halfdiamond*, mark options={solid, rotate=45}",
                                  "halfdiamond*, mark options={solid, rotate=135}",
                                  "halfcircle*, mark options={solid, rotate=180}",
                                  "halfcircle*, mark options={solid, rotate=270}",
                                  "halfcircle*, mark options={solid}",
                                  "halfcircle*, mark options={solid, rotate=90}",
                                  "halfcircle*, mark options={solid, rotate=225}",
                                  "halfcircle*, mark options={solid, rotate=315}",
                                  "halfcircle*, mark options={solid, rotate=45}",
                                  "halfcircle*, mark options={solid, rotate=135}",
                                  "halfsquare*, mark options={solid}",
                                  "halfsquare*, mark options={solid, rotate=90}",
                                  "halfsquare*, mark options={solid, rotate=180}",
                                  "halfsquare*, mark options={solid, rotate=270}",
                                  "halfsquare*, mark options={solid, rotate=45}",
                                  "halfsquare*, mark options={solid, rotate=135}",
                                  "halfsquare*, mark options={solid, rotate=225}",
                                  "halfsquare*, mark options={solid, rotate=315}",
                                  "*, mark options={solid}",
                                  "triangle*, mark options={solid}",
                                  "square*, mark options={solid, rotate=45}",
                                  "diamond, mark options=solid, rotate=90}",
                                  "diamond*, mark options={solid, rotate=90}",
                                  "diamond, mark options={solid, rotate=315}",
                                  "diamond*, mark options={solid, rotate=315}",
                                  "diamond, mark options={solid, rotate=45}",
                                  "diamond*, mark options={solid, rotate=45}",
                                  "Mercedes star, mark options={solid, rotate=180}",
                                  "square, mark options={solid}",
                                  "o, mark options={solid}",
                                  "triangle, mark options={solid}",
                                  "+, mark options={solid}",
                                  "x, mark options={solid}",
                                  "square, mark options={solid, rotate=45}",
                                  "triangle, mark options={solid, rotate=180}",
                                  "Mercedes star, mark options={solid}",
                                  "asterisk, mark options={solid}",
                                  "diamond, mark options={solid}",
                                  "oplus, mark options={solid}",
                                  "diamond*, mark options={solid}",
                                  "pentagon, mark options={solid}",
                                  "otimes, mark options={solid}",
                                  "pentagon*, mark options={solid}",
                                  "square*, mark options={solid}"
                                  ))

  ## Initialize blank output tex file
  fileConnection <- file(filename)
  writeLines("% This file was generated by R2pgf.\n", fileConnection)
  close(fileConnection)

  ## Look through ''gbuild'' and define all unique hex colours
  myColours <- character()
  if(class(ggbuild$data) == "list"){
    for(i in seq_along(ggbuild$data)){
      if(is.null(ggbuild$data[[i]]$colour) == F){
        myColours <- c(myColours, unique(ggbuild$data[[i]]$colour))
      }
    }
  } else{
    stop("Gotta have an option:")
  }
  myColours <- myColours[!(duplicated(myColours))]

  write("% Defined hexadecimal colours:", filename, append = T, sep = "")
  for(i in seq_along(myColours)){
    if(grepl("#", myColours[i], fixed = T) == T){
      write(paste0("\\definecolor{colour", i, "}{HTML}{", gsub("#", "", myColours[i]), "}%"), filename, append = T)
    } else if(myColours[i] == "black"){
      write(paste0("\\definecolor{colour", i, "}{HTML}{000000}%"), filename, append = T)
    } else{
      write(paste0("\\definecolor{colour", i, "}{", myColours[i], "}{1.0}%"), filename, append = T)
    }
  }
  write("", filename, append = T)

  ## Begin tikzpicture environment
  write("\\begin{tikzpicture}\n", filename, append = T, sep = "")

  ## Begin axis environment and parameters
  write("\\begin{axis}[%", filename, append = T, sep = "")

  ## Initializing axis dimensions and properties
  write("clip=true,", filename, append = T, sep = "")
  write("ticklabel style={font=\\LARGE},", filename, append = T, sep = "")
  write("label style={font=\\LARGE},", filename, append = T, sep = "")
  write("xlabel style={font=\\color{white!15!black}, font=\\LARGE},", filename, append = T, sep = "")
  write("ylabel style={font=\\color{white!15!black}, font=\\LARGE},", filename, append = T, sep = "")
  write(paste0("width=", width, "in,"), filename, append = T, sep = "")
  write(paste0("height=", height, "in,"), filename, append = T, sep = "")
  write("scale only axis,", filename, append = T, sep = "")
  write("xtick pos=left,", filename, append = T, sep = "")
  write("ytick pos=left,", filename, append = T, sep = "")

  ## Setting logarithm scales to false
  write("xmode=normal,", filename, append = T, sep = "")
  write("ymode=normal,", filename, append = T, sep = "")
  write("xminorticks=false,", filename, append = T, sep = "")
  write("yminorticks=false,", filename, append = T, sep = "")

  ## Setting axis ticks and tick labels
  write(paste0("xmin=", ggbuild$layout$panel_params[[1]]$x.range[1], ","), filename, append = T, sep = "")
  write(paste0("xmax=", ggbuild$layout$panel_params[[1]]$x.range[2], ","), filename, append = T, sep = "")
  write(paste0("ymin=", ggbuild$layout$panel_params[[1]]$y.range[1], ","), filename, append = T, sep = "")
  write(paste0("ymax=", ggbuild$layout$panel_params[[1]]$y.range[2], ","), filename, append = T, sep = "")
  xlab <- ggbuild$layout$panel_params[[1]]$x.labels
  ylab <- ggbuild$layout$panel_params[[1]]$y.labels
  if(xlabNotation == "exp"){
    #xlab <- g2$layout$panel_params[[1]]$x.labels
    #500 becomes 5 * 100 becomes 5 * 10^{2}
    #0.005 becomes 5 * 0.001 becomes 5 * 10^{-2}
    #paste0(xlab, "^{")
  } else{
    write(paste0("xticklabels={", paste(noquote(xlab), collapse=", "), "},"), filename, append = T, sep = "")
  }
  if(ylabNotation == "exp"){
  } else{
    write(paste0("yticklabels={", paste(noquote(ylab), collapse=", "), "},"), filename, append = T, sep = "")
  }

  write(paste0("xtick={", paste(noquote(xlab), collapse=", "), "},"), filename, append = T, sep = "")
  write(paste0("ytick={", paste(noquote(ylab), collapse=", "), "},"), filename, append = T, sep = "")

  ## Setting axis labels
  write(paste0("xlabel={", ggbuild$plot$labels$x,"},"), filename, append = T, sep = "")
  write(paste0("ylabel={", ggbuild$plot$labels$y,"},"), filename, append = T, sep = "")

  ## Setting background colour
  write("axis background/.style={fill=white},", filename, append = T, sep = "")

  ## Setting legend style
  if(legendLocation == "SE"){
    write(paste0("legend style={at={(0.97,0.03)}, anchor=south east, legend cell align=left, align=left, draw=white!15!black, font=\\LARGE}"), filename, append = T, sep = "")
  }

  ## End axis parameters
  write("]\n", filename, append = T, sep = "")

  ## Setting subfigure label
  # \node[anchor=south] at (axis cs:0,0.2) {subfigLab subfigPunc};


  ## Adding legend information in ggbuild$plot$data to ggbuild$data[[]] based on legendMat
  if(exists("legendMat") == T){
    for(i in 1:nrow(legendMat)){
      ggbuild$data[[as.numeric(legendMat[i, 2])]] <- as.data.table(ggbuild$data[[as.numeric(legendMat[i,2])]])
      ggbuild$data[[as.numeric(legendMat[i, 2])]][!(is.na(y))] <- ggbuild$data[[as.numeric(legendMat[i,2])]][is.na(y) == F]
      ggbuild$data[[as.numeric(legendMat[i, 2])]] <- merge(ggbuild$data[[as.numeric(legendMat[i,2])]], ggbuild$plot$data, by = c("x", "y"))
    }
    legendMat <- as.data.table(legendMat)
  }

  ## Loop over each ggplot2 layer and find list containing (presumably) linear regressions
  layers_lm <- numeric()
  for(layers in seq_along(ggbuild$data)){
    if(is.null(ggbuild$data[[layers]]$linetype) == F){
      layers_lm <- c(layers_lm, layers)
    } else{
      layers_lm <- c(layers_lm, NA)
    }
  }
  layers_lm <- layers_lm[!(is.na(layers_lm))]

  ## Loop over each ggplot2 layer and find list containing (presumably) text labels
  layers_text <- numeric()
  for(layers in seq_along(ggbuild$data)){
    if(is.null(ggbuild$data[[layers]]$label) == F){
      layers_text <- c(layers_text, layers)
    } else{
      layers_text <- c(layers_text, NA)
    }
  }
  layers_text <- layers_text[!(is.na(layers_text))]

  ## Loop over all data layers with lines (assumed linear regressions)
  if(length(layers_lm) > 0){

    write("% Linear regressions/vertical/horizontal lines:", filename, append = T)

    for(layers in seq_along(layers_lm)){
      ggbuild$data[[layers_lm[layers]]] <- as.data.table(ggbuild$data[[layers_lm[layers]]])

      if(any(ggbuild$data[[layers_lm[layers]]][, unique(linetype)] %in% "1")){

        ## geom_line assumption that we have vertical lines being plotted
        ggbuild$data[[layers_lm[layers]]][, alpha := ifelse(is.na(alpha) == T, 1, alpha)]
        ggbuild$data[[layers_lm[layers]]] <- ggbuild$data[[layers_lm[layers]]][is.na(x) == F & is.na(y) == F]

        naNames <- ggbuild$data[[layers_lm[layers]]][, unique(shape)]
        if(length(naNames) != 2){
          stop("I'm only expecting 2 shapes!")
        }

        ggbuild$data[[layers_lm[layers]]] <- dcast(ggbuild$data[[layers_lm[layers]]],
                                                   colour + x + group + size + linetype + alpha ~ shape, value.var = "y")
        naNames <- which(colnames(ggbuild$data[[layers_lm[layers]]]) %in% naNames)

        ggbuild$data[[layers_lm[layers]]][, "remove" := rowSums(is.na(ggbuild$data[[layers_lm[layers]]][, naNames, with=F]))]
        ggbuild$data[[layers_lm[layers]]] <- ggbuild$data[[layers_lm[layers]]][remove == 0]

        ## Converting line types in R to pgfplots
        ggbuild$data[[layers_lm[layers]]][, linetype := ifelse(linetype == as.character(1), "solid", linetype)]

        ## Loop over all groups
        for(groups in 1:nrow(ggbuild$data[[layers_lm[layers]]])){
          which(myColours == ggbuild$data[[layers_lm[layers]]][groups, colour]) # color=
          write(paste0("\\draw[color=", myColours[which(myColours == ggbuild$data[[layers_lm[layers]]][groups, colour])], ", ", ggbuild$data[[layers_lm[layers]]][groups, linetype], ", linewidth=", ggbuild$data[[layers_lm[layers]]][groups, size], ", draw opacity=", ggbuild$data[[layers_lm[layers]]][groups, alpha], "] ", "(", ggbuild$data[[layers_lm[layers]]][groups, x], ", ", ggbuild$data[[layers_lm[layers]]][groups, naNames[1], with=F], ") -- (", ggbuild$data[[layers_lm[layers]]][groups, x], ", ", ggbuild$data[[layers_lm[layers]]][groups, naNames[2], with=F], ");"), filename, append = T, sep = "")
        }
      } else{
        ## Back-calculate the linear regression coefficients from smallest/biggest
        ## domain values for each group
        ggbuild$data[[layers_lm[layers]]] <- as.data.table(ggbuild$data[[layers_lm[layers]]])
        ggbuild$data[[layers_lm[layers]]][, `:=` (int=coef(lm(y~x))[1],
                                                  slope=coef(lm(y~x))[2]),
                                          by=c("group")]
        ggbuild$data[[layers_lm[layers]]][, `:=` (minGroup=ifelse(x == min(x), 1, 0),
                                                  maxGroup=ifelse(x == max(x), 1, 0)),
                                          by=c("group")]
        ggbuild$data[[layers_lm[layers]]][, alpha := ifelse(is.na(alpha) == T, 1, alpha)]

        ## Only keep rows with smallest/biggest domain values and cast/reshape
        ggbuild$data[[layers_lm[layers]]] <- ggbuild$data[[layers_lm[layers]]][minGroup == 1 | maxGroup == 1]
        ggbuild$data[[layers_lm[layers]]] <- dcast(ggbuild$data[[layers_lm[layers]]],
                                                   colour + linetype + alpha + size + int + slope + group ~ maxGroup, value.var = "x")

        ## Write linear regression models to file
        for(i in 1:nrow(ggbuild$data[[layers_lm[layers]]])){
          temp <- ggbuild$data[[layers_lm[layers]]]
          ## Legend entries must be configured manually and 'forget plot' removed
          write(paste0("\\addplot[domain=", temp[i, "0"], ":", temp[i, "1"],  ", color=black, line width=", temp[i, size], "mm, opacity=", temp[i, alpha], ", forget plot]{", temp[i, slope], "*x + ", temp[i, int], "};"), filename, append = T, sep = "")
        }
        rm(temp)
      }
    }
    write("", filename, append = T, sep = "")
  }

  ## Loop over each ggplot2 layer (no label layers though!) and plot points by group
  layers_scatter <- seq_along(ggbuild$data)[!(seq_along(ggbuild$data) %in% c(layers_lm, layers_text))]

  for(layers in seq_along(layers_scatter)){

    ## Loop over 'group' which is assumed to be the master group separator
    for(groups in seq_along(unique(ggbuild$data[[layers_scatter[layers]]]$group))){
      groupDT <- as.data.table(ggbuild$data[[layers_scatter[layers]]])
      groupDT <- groupDT[group == unique(group)[groups]]
      groupDT <- groupDT[is.na(y) == F]
      groupDT[, alpha := ifelse(is.na(alpha) == T, "1.0", as.character(alpha))]

      if(nrow(groupDT) > 0){
        markerTable[R == groupDT[, unique(shape)], pgf] # mark=
        which(myColours == groupDT[, unique(colour)]) # color=
        groupDT[, unique(size)] # mark size= pt
        groupDT[, unique(fill)] # Unused...
        groupDT[, unique(alpha)] # fill opacity=
        groupDT[, unique(stroke)] # line width= pt

        write(paste0("\\addplot[color=colour", which(myColours == groupDT[, unique(colour)]), ", draw=none, mark size=", groupDT[, unique(size)], "pt, mark=", markerTable[R == groupDT[, unique(shape)], pgf], ", only marks, fill opacity=", groupDT[, unique(alpha)], ", line width=", groupDT[, unique(stroke)], "pt]"), filename, append = T, sep = "")
        write("\ttable[row sep=crcr]{%", filename, append = T, sep = "")

        ## Loop over each individual row in a group
        for(row in 1:nrow(groupDT)){
          write(paste0(groupDT[row, x], "\t", groupDT[row, y], "\\\\"), filename, append = T, sep = "")
        }
        write("};", filename, append = T, sep = "")

        if(exists("legendMat")){
          e <- parse(text=legendMat[V2 == layers_scatter[layers], V1])
          write(paste0("\\addlegendentry{", groupDT[, unique(eval(e))], "}"), filename, append = T, sep = "")
        }
        write("\n", filename, append = T, sep = )
      }
    }
  }

  #write("", filename, append = T, sep = "")

  ## End axis environment
  write("\\end{axis}", filename, append = T, sep = "")

  ## End tikzpicture environment
  write("\\end{tikzpicture}%", filename, append = T, sep = "")

  invisible(silentReturn <- NULL)
}
