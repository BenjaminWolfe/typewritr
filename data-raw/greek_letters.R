## code to prepare `greek_letters` dataset goes here
greek_letters <-
  tibble::tribble(
    ~label,	~lowercase,	~uppercase,
    "alpha"      ,	"α",	"Α",
    "beta"       ,	"β",	"Β",
    "gamma"      ,	"γ",	"Γ",
    "delta"      ,	"δ",	"Δ",
    "epsilon"    ,	"ε",	"Ε",
    "zeta"       ,	"ζ",	"Ζ",
    "eta"        ,	"η",	"Η",
    "theta"      ,	"θ",	"Θ",
    "iota"       ,	"ι",	"Ι",
    "kappa"      ,	"κ",	"Κ",
    "lambda"     ,	"λ",	"Λ",
    "mu"         ,	"μ",	"Μ",
    "nu"         ,	"ν",	"Ν",
    "xi"         ,	"ξ",	"Ξ",
    "omicron"    ,	"ο",	"Ο",
    "pi"         ,	"π",	"Π",
    "rho"        ,	"ρ",	"Ρ",
    "sigma"      ,	"σ",	"Σ",
    "sigma.final",	"ς",	NA ,
    "tau"        ,	"τ",	"Τ",
    "upsilon"    ,	"υ",	"Υ",
    "phi"        ,	"φ",	"Φ",
    "chi"        ,	"χ",	"Χ",
    "psi"        ,	"ψ",	"Ψ",
    "omega"      ,	"ω",	"Ω"
  )

usethis::use_data(greek_letters, overwrite = TRUE)
