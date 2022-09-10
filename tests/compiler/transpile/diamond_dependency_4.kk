module (bar) = import "./diamond_dependency_2.kk"
module (spam) = import "./diamond_dependency_3.kk"

entry
  bar () print;
  spam () print
