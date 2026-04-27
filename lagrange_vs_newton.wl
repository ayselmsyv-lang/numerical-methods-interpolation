data = {{1, 2}, {2, 3}, {4, 7}, {5, 11}};

xVals = data[[All, 1]];
yVals = data[[All, 2]];
n = Length[data];

(*Lagrange interpolation*)
lagrange[x_] := 
  Sum[yVals[[i]]*
    Product[If[j != i, (x - xVals[[j]])/(xVals[[i]] - xVals[[j]]), 
      1], {j, 1, n}], {i, 1, n}];

(*Newton divided differences*)
divdiff[i_, 1] := yVals[[i]];
divdiff[i_, 
   j_] := (divdiff[i + 1, j - 1] - 
     divdiff[i, j - 1])/(xVals[[i + j - 1]] - xVals[[i]]);

newton[x_] := 
  divdiff[1, 1] + 
   Sum[divdiff[1, k]*Product[x - xVals[[m]], {m, 1, k - 1}], {k, 2, 
     n}];

Print["Lagrange polynomial = ", Expand[lagrange[x]]];
Print["Newton polynomial = ", Expand[newton[x]]];
Print["Are they equal? ", Expand[lagrange[x]] == Expand[newton[x]]];

Print["Value at x=3 using Lagrange: ", N[lagrange[3]]];
Print["Value at x=3 using Newton: ", N[newton[3]]];

Show[ListPlot[data, PlotStyle -> Red, PlotMarkers -> Automatic], 
 Plot[{lagrange[x], newton[x]}, {x, Min[xVals], Max[xVals]}, 
  PlotLegends -> {"Lagrange", "Newton"}]]

(*ListPlot -> draws the dots
Plot -> draws the curve*)
