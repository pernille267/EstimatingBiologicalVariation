# Documentation Module
# ------------------------------------------------------------------------------
#
# Displays application documentation and model descriptions.
#
# ------------------------------------------------------------------------------

# ── Section builder helpers ───────────────────────────────────────────────────

#' Build a documentation section with header and body
#' @keywords internal
.doc_section <- function(..., header = NULL) {
    tagList(
        if (!is.null(header)) div(class = "glass-help-header", header),
        div(class = "glass-help-section", ...)
    )
}

#' Build a tip box
#' @keywords internal
.doc_tip <- function(text) {
    div(class = "glass-help-tip",
        icon("lightbulb"),
        span(text)
    )
}

#' Build an info box with a coloured left border
#' @keywords internal
.doc_info_box <- function(..., colour = "#605CA8") {
    div(class = "glass-help-info-box",
        style = paste0("border-left-color:", colour, ";"),
        ...
    )
}

#' Build an info item row
#' @keywords internal
.doc_info_item <- function(...) {
    div(class = "glass-help-info-item", ...)
}

# ── Section content builders ─────────────────────────────────────────────────

.doc_section_overview <- function() {
    .doc_section(
        header = "1. Overview",
        p("This application estimates",
          tags$strong("biological variation (BV)"),
          "components from repeated-measures laboratory data using Bayesian hierarchical models
           fitted via Hamiltonian Monte Carlo (HMC) in Stan. It also provides classical
           nested ANOVA estimates for comparison."
        ),
        p("The core quantities of interest are:"),
        tags$ul(
            tags$li("\\(\\mathrm{CV}_G\\) \u2014 the between-subject coefficient of variation,"),
            tags$li("\\(\\mathrm{CV}_I\\) \u2014 the within-subject (biological) coefficient of variation,"),
            tags$li("\\(\\mathrm{CV}_A\\) \u2014 the analytical coefficient of variation,"),
            tags$li("RCV \u2014 reference change values derived from the above.")
        ),
        p("Bayesian estimation lets us incorporate prior knowledge, model heavy-tailed
           variation, account for heterogeneity of within-subject variation across
           individuals, and obtain full posterior distributions (with credible intervals)
           for every parameter.")
    )
}

.doc_section_data_model <- function() {
    .doc_section(
        header = "2. The Observation Model",
        p("Each measurement \\(y_{isr}\\) for subject \\(i\\), sampling occasion \\(s\\),
           and replicate \\(r\\) is decomposed as"),
        p(style = "text-align:center;",
          "\\[y_{isr} \\;=\\; \\beta \\;+\\; G_i \\;+\\; I_{is} \\;+\\; A_{isr}\\]"
        ),

        .doc_info_box(
            colour = "#605CA8",
            .doc_info_item(tags$strong("\\(\\beta\\)"),
                           "\u2003Grand mean \u2014 the overall expected concentration in the population."),
            .doc_info_item(tags$strong("\\(G_i\\)"),
                           "\u2003Between-subject effect \u2014 the permanent deviation of subject \\(i\\) from \\(\\beta\\)."),
            .doc_info_item(tags$strong("\\(I_{is}\\)"),
                           "\u2003Within-subject effect \u2014 the random fluctuation of subject \\(i\\) at occasion \\(s\\) around their set-point."),
            .doc_info_item(tags$strong("\\(A_{isr}\\)"),
                           "\u2003Analytical error \u2014 measurement noise on replicate \\(r\\).")
        ),
        p("The", tags$em("homeostatic set-point"), "of subject \\(i\\) is
           \\(\\mu_i = \\beta + G_i\\). All variation beyond this set-point is
           partitioned into biological fluctuation \\(I_{is}\\) and analytical noise \\(A_{isr}\\)."),
        p("This is a", tags$strong("nested random-effects model:"),
          "replicates are nested within samples, which are nested within subjects.")
    )
}

.doc_section_distributions <- function() {
    .doc_section(
        header = "3. Distribution Choices \u2014 Model Naming Convention",
        p("The three random components \\((G,\\, I,\\, A)\\) can each follow a Normal distribution
           or a location-scale Student-\\(t\\) distribution. The three-letter model code encodes
           these choices:"),

        .doc_info_box(
            colour = "#3c8dbc",
            .doc_info_item(tags$strong("N"), "\u2003Normal distribution"),
            .doc_info_item(tags$strong("T"), "\u2003Location-scale Student-\\(t\\) distribution")
        ),

        p("The three positions correspond to \\(G\\), \\(I\\), \\(A\\) in that order.
           The between-subject component \\(G\\) always uses a Normal distribution
           (the first letter is always N), giving five model variants:"),

        tags$table(
            class = "table table-striped",
            style = "max-width:600px;",
            tags$thead(
                tags$tr(
                    tags$th("Model"), tags$th("\\(G_i\\)"), tags$th("\\(I_{is}\\)"), tags$th("\\(A_{isr}\\)"), tags$th("Note")
                )
            ),
            tags$tbody(
                tags$tr(tags$td("NNN"), tags$td("Normal"), tags$td("Normal"), tags$td("Normal"), tags$td("Simplest; equivalent to classical ANOVA assumptions")),
                tags$tr(tags$td("NNT"), tags$td("Normal"), tags$td("Normal"), tags$td("Student-\\(t\\)"), tags$td("Heavy tails for analytical error only")),
                tags$tr(tags$td("NTN"), tags$td("Normal"), tags$td("Student-\\(t\\)"), tags$td("Normal"), tags$td("Heavy tails for within-subject BV only")),
                tags$tr(tags$td("NTT"), tags$td("Normal"), tags$td("Student-\\(t\\)"), tags$td("Student-\\(t\\)"), tags$td("Heavy tails for both \\(I\\) and \\(A\\)")),
                tags$tr(tags$td("NTTDFGAM"), tags$td("Normal"), tags$td("Student-\\(t\\)"), tags$td("Student-\\(t\\)"), tags$td("As NTT, but Gamma prior on degrees-of-freedom"))
            )
        ),

        h4("Why use Student-\\(t\\)?"),
        p("The Student-\\(t\\) distribution has heavier tails than the Normal and is controlled by a
           degrees-of-freedom parameter \\(\\nu\\). As \\(\\nu \\to \\infty\\) the Student-\\(t\\)
           converges to the Normal, so the Normal model is a special case. When \\(\\nu\\) is finite
           the model accommodates occasional extreme values without inflating the estimated variance.
           This makes the analysis more", tags$strong("robust to outliers"), ".")
    )
}

.doc_section_lst <- function() {
    .doc_section(
        header = "4. Location-Scale Student-\\(t\\) Parameterisation",
        p("A standard Student-\\(t\\) with \\(\\nu\\) degrees of freedom and scale \\(s\\) has
           variance \\(s^2 \\,\\nu\\,/\\,(\\nu - 2)\\) for \\(\\nu > 2\\). We want the parameter
           \\(\\sigma\\) in our model to represent the", tags$em("standard deviation"),
          "(not just the scale). This requires a rescaling."),

        h4("4.1 Variance-preserving rescaling"),
        p("Suppose we want \\(X\\) to have zero mean and standard deviation \\(\\sigma\\).
           We generate"),
        p(style = "text-align:center;",
          "\\[X_{\\text{raw}} \\sim t_{\\nu}(0,\\, 1)\\]"
        ),
        p("and set"),
        p(style = "text-align:center;",
          "\\[X \\;=\\; X_{\\text{raw}} \\;\\cdot\\;
            \\sigma\\,\\sqrt{\\frac{\\nu - 2}{\\nu}}\\]"
        ),
        p("because"),
        p(style = "text-align:center;",
          "\\[\\operatorname{Var}(X) \\;=\\;
            \\sigma^2\\,\\frac{\\nu - 2}{\\nu}\\;\\cdot\\;\\frac{\\nu}{\\nu - 2}
            \\;=\\; \\sigma^2.\\]"
        ),
        p("This is applied to both \\(I_{is}\\) and \\(A_{isr}\\) when they use a Student-\\(t\\) distribution.
           Concretely:"),
        p(style = "text-align:center;",
          "\\[I_{is} \\sim t_{\\nu_I}\\!\\left(0,\\;
            \\sigma_i\\,\\sqrt{\\tfrac{\\nu_I - 2}{\\nu_I}}\\right),
            \\qquad
            A_{isr} \\sim t_{\\nu_A}\\!\\left(0,\\;
            \\sigma_A\\,\\sqrt{\\tfrac{\\nu_A - 2}{\\nu_A}}\\right)\\]"
        ),
        p("so that \\(\\operatorname{Var}(I_{is}) = \\sigma_i^2\\) and
           \\(\\operatorname{Var}(A_{isr}) = \\sigma_A^2\\) regardless of the degrees of freedom."),

        h4("4.2 The \\(\\nu - 2\\) parameterisation"),
        p("Since the variance only exists for \\(\\nu > 2\\), we parameterise the
           sampled quantity as \\(\\delta = \\nu - 2\\) with \\(\\delta \\ge 0.1\\),
           guaranteeing \\(\\nu \\ge 2.1\\). The prior is placed on \\(\\delta\\) and the
           actual degrees of freedom are recovered as \\(\\nu = \\delta + 2\\).")
    )
}

.doc_section_hierarchical_sigma <- function() {
    .doc_section(
        header = "5. Heterogeneous Within-Subject Variation",
        p("A key feature of the Bayesian model is that each subject has their own
           within-subject standard deviation \\(\\sigma_i\\), drawn from a
           population-level distribution:"),
        p(style = "text-align:center;",
          "\\[\\sigma_i \\;\\sim\\; \\mathcal{N}^{+}\\!\\left(\\mu_{\\sigma_I},\\;
            \\tau_{\\sigma_I}\\right), \\qquad i = 1,\\dots,N_{\\text{subj}}\\]"),
        p("where \\(\\mathcal{N}^{+}\\) denotes a Normal truncated below at zero,
           and the hyperparameters are:"),
        tags$ul(
            tags$li("\\(\\mu_{\\sigma_I}\\) \u2014 the population expected value of within-subject SD,"),
            tags$li("\\(\\tau_{\\sigma_I}\\) \u2014 the population SD of within-subject SDs.")
        ),
        p("This hierarchy captures",
          tags$strong("biological heterogeneity:"),
          "different subjects genuinely have different amounts of biological fluctuation.
           The degree of heterogeneity is summarised by the",
          tags$em("Heterogeneity of Biological Variation Ratio"), "(HBHR):"),
        p(style = "text-align:center;",
          "\\[\\text{HBHR} \\;=\\; \\frac{\\tau_{\\sigma_I}}{\\mu_{\\sigma_I}}
            \\times 100\\%\\]"),
        p("An HBHR of 0% means all subjects have identical biological variation;
           larger values indicate greater inter-individual differences."),

        h4("Subject-specific vs.\u00a0population-level inference"),
        p("Because each \\(\\sigma_i\\) is estimated jointly with the
           hyperparameters, the model produces:"),
        tags$ul(
            tags$li(tags$strong("Subject-specific posteriors:"),
                    "\\(\\sigma_i\\) (and hence \\(\\mathrm{CV}_{I,i}\\)) for each observed subject."),
            tags$li(tags$strong("Population-level posteriors:"),
                    "\\(\\mu_{\\sigma_I}\\) and \\(\\tau_{\\sigma_I}\\), summarising the
                     distribution of biological variation in the population."),
            tags$li(tags$strong("Predictive distribution:"),
                    "For a", tags$em("new, unobserved"), "subject, a predictive
                     \\(\\sigma_{I,\\text{pred}}\\) is drawn from
                     \\(\\mathcal{N}^{+}(\\mu_{\\sigma_I},\\,\\tau_{\\sigma_I})\\)
                     in each MCMC iteration to represent uncertainty about future individuals.")
        )
    )
}

.doc_section_priors <- function() {
    .doc_section(
        header = "6. Prior Specification",
        p("Priors are specified on a clinically interpretable scale and automatically
           translated to the Stan hyperparameters. The user provides:"),

        .doc_info_box(
            colour = "#00a65a",
            .doc_info_item(tags$strong("\\(\\beta\\)"), "\u2003Expected grand mean concentration."),
            .doc_info_item(tags$strong("\\(\\mathrm{CV}_I\\,\\%\\)"), "\u2003Expected within-subject CV."),
            .doc_info_item(tags$strong("\\(\\mathrm{CV}_A\\,\\%\\)"), "\u2003Expected analytical CV."),
            .doc_info_item(tags$strong("\\(\\mathrm{CV}_G\\,\\%\\)"), "\u2003Expected between-subject CV."),
            .doc_info_item(tags$strong("\\(\\mathrm{df}_I,\\;\\mathrm{df}_A\\)"), "\u2003Expected degrees-of-freedom (when using Student-\\(t\\) components)."),
            .doc_info_item(tags$strong("HBHR\\(\\,\\%\\)"), "\u2003Expected heterogeneity ratio."),
            .doc_info_item(tags$strong("Weakness"), "\u2003A multiplier controlling how diffuse each prior is.")
        ),

        h4("6.1 Translation to SD-scale priors (identity scale)"),
        p("When the analysis is on the original (identity) scale, the user-supplied CVs are
           converted to standard deviations through the grand mean:"),
        p(style = "text-align:center;",
          "\\[\\sigma_I = \\frac{\\mathrm{CV}_I}{100}\\,\\beta, \\qquad
            \\sigma_A = \\frac{\\mathrm{CV}_A}{100}\\,\\beta, \\qquad
            \\sigma_G = \\frac{\\mathrm{CV}_G}{100}\\,\\beta\\]"),

        h4("6.2 Translation to SD-scale priors (log scale)"),
        p("When the data are log-transformed before fitting, CVs map to log-scale SDs via:"),
        p(style = "text-align:center;",
          "\\[\\sigma = \\sqrt{\\ln\\!\\left(1 + \\left(\\frac{\\mathrm{CV}}{100}\\right)^{\\!2}\\right)}\\]"),
        p("and the prior mean for \\(\\beta\\) is set to \\(\\ln(\\beta)\\)."),

        h4("6.3 Prior distributions on Stan parameters"),
        p("All variance-component priors are half-Normal (truncated at zero through the
           parameter constraint \\(\\sigma > 0\\)):"),
        p(style = "text-align:center;",
          "\\[\\sigma_G \\sim \\mathcal{N}^{+}\\!\\left(\\sigma_{G,0},\\;
            w_G \\cdot \\sigma_{G,0}\\right)\\]"),
        p(style = "text-align:center;",
          "\\[\\mu_{\\sigma_I} \\sim \\mathcal{N}^{+}\\!\\left(\\sigma_{I,0},\\;
            w_I \\cdot \\sigma_{I,0}\\right) \\qquad
            \\tau_{\\sigma_I} \\sim \\mathcal{N}^{+}\\!\\left(
            \\text{HBHR}\\cdot\\sigma_{I,0},\\;
            w_{\\text{HBHR}} \\cdot \\sigma_{I,0}\\right)\\]"),
        p(style = "text-align:center;",
          "\\[\\sigma_A \\sim \\mathcal{N}^{+}\\!\\left(\\sigma_{A,0},\\;
            w_A \\cdot \\sigma_{A,0}\\right)\\]"),
        p(style = "text-align:center;",
          "\\[\\beta \\sim \\mathcal{N}\\!\\left(\\beta_0,\\;
            w_\\beta \\cdot |\\beta_0|\\right)\\]"),
        p("where \\(w_\\bullet\\) is the", tags$em("weakness"), "multiplier for that parameter
           and the subscript 0 denotes the prior expectation. The grand mean \\(\\beta\\) is
           not truncated because negative means are valid on the log scale."),

        h4("6.4 Degrees-of-freedom priors"),
        p("The prior is placed on \\(\\delta = \\nu - 2\\), with \\(\\delta \\ge 0.1\\):"),
        tags$ul(
            tags$li(tags$strong("NTT model:"),
                    "\\(\\delta \\sim \\mathcal{N}^{+}\\!(\\mathrm{df}_0 - 2,\\;
                     w_{\\mathrm{df}} \\cdot \\mathrm{df}_0)\\), i.e.\u00a0a (truncated)
                     Normal prior."),
            tags$li(tags$strong("NTTDFGAM model:"),
                    "\\(\\delta \\sim \\text{Gamma}(\\alpha,\\,\\lambda)\\), where \\(\\alpha\\) and
                     \\(\\lambda\\) are set so that
                     \\(\\mathrm{E}[\\delta] = \\mathrm{df}_0 - 2\\) and
                     \\(\\mathrm{SD}[\\delta] = w_{\\mathrm{df}} \\cdot \\mathrm{df}_0\\).
                     The Gamma naturally respects \\(\\delta > 0\\) without truncation.")
        ),
        .doc_tip("Larger weakness values produce wider, more diffuse priors that let the data
                  dominate. A weakness of 1 centres the prior SD at roughly the same magnitude
                  as the prior mean; values around 0.1\u20130.5 encode moderate prior confidence.")
    )
}

.doc_section_cv <- function() {
    .doc_section(
        header = "7. Coefficient of Variation (CV) Calculations",
        p("The CV expresses variability as a percentage of the mean. Its computation
           depends on the measurement scale and the distributional assumption."),

        h4("7.1 Identity scale"),
        p("For a Normal component with standard deviation \\(\\sigma\\) and overall mean \\(\\beta\\):"),
        p(style = "text-align:center;",
          "\\[\\mathrm{CV} = \\frac{\\sigma}{\\beta} \\times 100\\]"),
        p("For a Student-\\(t\\) component, the SD is larger than the scale parameter:"),
        p(style = "text-align:center;",
          "\\[\\mathrm{SD} = \\sigma\\,\\sqrt{\\frac{\\nu}{\\nu - 2}},
            \\qquad
            \\mathrm{CV} = \\frac{\\mathrm{SD}}{\\beta} \\times 100\\]"),
        p("Subject-specific CVs use the individual set-point \\(\\mu_i = \\beta + G_i\\) as denominator:"),
        p(style = "text-align:center;",
          "\\[\\mathrm{CV}_{I,i} = \\frac{\\sigma_i\\,\\sqrt{\\nu_I/(\\nu_I - 2)}}{\\beta + G_i}
            \\times 100\\]"),

        h4("7.2 Log scale"),
        p("When the data are log-transformed, the model parameters live on the log scale.
           CVs on the original scale are recovered through the log-normal (or log-\\(t\\))
           relationship."),
        p("For a log-Normal component with log-scale SD \\(\\sigma\\):"),
        p(style = "text-align:center;",
          "\\[\\mathrm{CV} = \\sqrt{\\exp(\\sigma^2) - 1} \\times 100\\]"),
        p("For a log-Student-\\(t\\) component:"),
        p(style = "text-align:center;",
          "\\[\\mathrm{CV} = \\sqrt{\\exp\\!\\left(\\sigma^2 \\cdot
            \\frac{\\nu}{\\nu - 2}\\right) - 1} \\times 100\\]"),
        p("The between-subject CV is always log-Normal (\\(G\\) is Normal):"),
        p(style = "text-align:center;",
          "\\[\\mathrm{CV}_G = \\sqrt{\\exp(\\sigma_G^2) - 1} \\times 100\\]"),
        p("Back-transformed quantities on the original scale:"),
        p(style = "text-align:center;",
          "\\[\\hat{\\beta}_{\\text{orig}} = \\exp(\\beta), \\qquad
            \\mu_i = \\exp(\\beta + G_i)\\]"),

        .doc_tip("On the log scale the CV formulae do not depend on the mean, making them
                  natural for analytes whose variability scales with concentration.")
    )
}

.doc_section_rcv <- function() {
    .doc_section(
        header = "8. Reference Change Value (RCV)",
        p("The RCV quantifies the minimum percentage change between two consecutive measurements
           from the same individual that is unlikely to be due to random variation alone.
           It combines analytical and within-subject biological variation."),

        h4("8.1 Standard (symmetric) RCV"),
        p("Assuming Normally distributed changes, the difference between two independent
           measurements from the same subject has variance
           \\(2\\,(\\sigma_A^2 + \\sigma_I^2)\\). At the 95% probability level:"),
        p(style = "text-align:center;",
          "\\[\\mathrm{RCV} = \\pm\\, z_{0.975}\\,\\sqrt{2}\\,
            \\sqrt{\\mathrm{CV}_A^2 + \\mathrm{CV}_{I}^2}\\]"),
        p("where \\(z_{0.975} \\approx 1.96\\). This is a symmetric interval: the threshold for
           a significant increase equals the threshold for a significant decrease in absolute value."),

        h4("8.2 Log-based (asymmetric) RCV"),
        p("When variability is proportional to the level (as is common for many analytes),
           changes on the log scale are more appropriate. From log-normal theory:"),
        p(style = "text-align:center;",
          "\\[\\sigma_{\\ln} = \\sqrt{\\ln\\!\\left(
            \\left(\\frac{\\mathrm{CV}_A}{100}\\right)^{\\!2} +
            \\left(\\frac{\\mathrm{CV}_I}{100}\\right)^{\\!2} + 1\\right)}\\]"),
        p(style = "text-align:center;",
          "\\[\\mathrm{RCV}_{\\text{upper}} =
            \\left(\\exp\\!\\left(z_{0.975}\\,\\sqrt{2}\\,\\sigma_{\\ln}\\right) - 1\\right)
            \\times 100\\%\\]"),
        p(style = "text-align:center;",
          "\\[\\mathrm{RCV}_{\\text{lower}} =
            \\left(\\exp\\!\\left(-z_{0.975}\\,\\sqrt{2}\\,\\sigma_{\\ln}\\right) - 1\\right)
            \\times 100\\%\\]"),
        p("The asymmetric RCV recognises that a 20% increase and a 20% decrease are not
           symmetric on the original scale: a value that increases by 20% and then decreases by
           20% does not return to its starting point."),

        h4("8.3 Subject-level vs.\u00a0population-level RCV"),
        p("The RCV can be evaluated at two levels:"),
        tags$ul(
            tags$li(tags$strong("Subject-level:"),
                    "Uses the subject-specific \\(\\mathrm{CV}_{I,i}\\), giving an RCV
                     tailored to that individual\u2019s biological variability."),
            tags$li(tags$strong("Population-level:"),
                    "Uses the population \\(\\mathrm{CV}_I\\) (from \\(\\mu_{\\sigma_I}\\)),
                     representing the expected RCV for a typical individual from the population.")
        ),
        .doc_tip("Because the Bayesian model yields a full posterior for each subject's
                  \\(\\mathrm{CV}_{I,i}\\), the subject-level RCV comes with a credible interval,
                  quantifying uncertainty about that individual's true threshold.")
    )
}

.doc_section_bayesian_inference <- function() {
    .doc_section(
        header = "9. Bayesian Inference \u2014 From Prior to Posterior",
        p("The Bayesian approach combines prior information with the observed data
           via Bayes' theorem:"),
        p(style = "text-align:center;",
          "\\[p(\\boldsymbol{\\theta} \\mid \\mathbf{y})
            \\;\\propto\\;
            p(\\mathbf{y} \\mid \\boldsymbol{\\theta})\\;
            p(\\boldsymbol{\\theta})\\]"),
        p("where \\(\\boldsymbol{\\theta}\\) collects all model parameters
           \\((\\beta,\\,\\sigma_G,\\,\\mu_{\\sigma_I},\\,\\tau_{\\sigma_I},\\,
           \\sigma_A,\\,\\nu_I,\\,\\nu_A,\\,\\{G_i\\},\\,\\{\\sigma_i\\},\\,\\{I_{is}\\})\\)."),

        h4("9.1 The likelihood"),
        p("The likelihood factorises over all observations. Writing the
           conditional mean as \\(\\hat{y}_{isr} = \\beta + G_i + I_{is}\\):"),

        .doc_info_box(
            colour = "#f39c12",
            .doc_info_item(tags$strong("Normal analytical error (models NNN, NTN):"),
                           "\\(y_{isr} \\mid \\hat{y}_{isr},\\,\\sigma_A
                            \\;\\sim\\; \\mathcal{N}(\\hat{y}_{isr},\\,\\sigma_A)\\)"),
            .doc_info_item(tags$strong("Student-\\(t\\) analytical error (models NNT, NTT, NTTDFGAM):"),
                           "\\(y_{isr} \\mid \\hat{y}_{isr},\\,\\sigma_A,\\,\\nu_A
                            \\;\\sim\\; t_{\\nu_A}\\!\\left(\\hat{y}_{isr},\\;
                            \\sigma_A\\sqrt{\\tfrac{\\nu_A - 2}{\\nu_A}}\\right)\\)")
        ),
        p("The within-subject effects similarly follow either
           \\(I_{is} \\sim \\mathcal{N}(0, \\sigma_i)\\) or the variance-preserving
           Student-\\(t\\), depending on the model."),

        h4("9.2 The prior"),
        p("Section\u00a06 details every prior. The full prior is the product of all
           individual parameter priors. By adjusting the", tags$em("weakness"),
          "multiplier the user controls how strongly each prior constrains the
           parameter relative to the data."),

        h4("9.3 The posterior"),
        p("The posterior distribution \\(p(\\boldsymbol{\\theta} \\mid \\mathbf{y})\\)
           is the updated belief about all parameters after seeing the data.
           It is not available in closed form, so we approximate it by drawing
           samples using", tags$strong("Hamiltonian Monte Carlo"), "(HMC) via Stan.
           Each MCMC sample is a full realisation of \\(\\boldsymbol{\\theta}\\);
           summaries (medians, credible intervals) are computed from these draws."),
        p("What makes the posterior especially useful is that derived quantities
           (CVs, RCVs, etc.) can be computed from", tags$em("each"), "posterior draw,
           automatically propagating uncertainty through all non-linear transformations."),

        h4("9.4 Posterior predictive quantities"),
        p("In each MCMC iteration the model also draws a", tags$em("predictive"),
          "within-subject SD for a new, hypothetical subject:"),
        p(style = "text-align:center;",
          "\\[\\sigma_{I,\\text{pred}} \\sim
            \\mathcal{N}^{+}\\!(\\mu_{\\sigma_I},\\,\\tau_{\\sigma_I})\\]"),
        p("When the within-subject component uses a Student-\\(t\\), the raw predictive draw
           is converted to the SD convention by multiplying by
           \\(\\sqrt{\\nu_I/(\\nu_I - 2)}\\). The predictive distribution of
           \\(\\mathrm{CV}_{I,\\text{pred}}\\) represents the uncertainty about
           what CV a future, as-yet-unobserved individual might have.")
    )
}

.doc_section_anova_comparison <- function() {
    .doc_section(
        header = "10. Classical ANOVA Comparison",
        p("The application also provides classical nested ANOVA estimates as a
           non-Bayesian benchmark. The same observation model applies:"),
        p(style = "text-align:center;",
          "\\[y_{isr} = \\mu + G_i + I_{is} + A_{isr}\\]"),
        p("with all components Normal. Variance components are estimated by equating
           observed mean squares to their expected values:"),
        p(style = "text-align:center;",
          "\\[\\hat{\\sigma}_A^2 = \\mathrm{MS}_{\\text{within}},
            \\qquad
            \\hat{\\sigma}_I^2 = \\frac{\\mathrm{MS}_{\\text{between samples}}
            - \\mathrm{MS}_{\\text{within}}}{\\bar{n}_r},
            \\qquad
            \\hat{\\sigma}_G^2 = \\frac{\\mathrm{MS}_{\\text{between subjects}}
            - c_2\\,\\mathrm{MS}_{\\text{between samples}}
            + c_3\\,\\mathrm{MS}_{\\text{within}}}{\\bar{n}_s}\\]"),
        p("where \\(\\bar{n}_r\\), \\(\\bar{n}_s\\), \\(c_2\\), \\(c_3\\) are weighting
           coefficients that correct for unbalanced designs."),

        h4("Confidence intervals"),
        p("Confidence intervals for the variance components use",
          tags$strong("Burdick\u2013Graybill (1992) generalised confidence intervals"),
          "based on \\(F\\)-distribution pivots or, alternatively,",
          tags$strong("bootstrap"), "percentile intervals obtained by resampling subjects
           with replacement."),

        h4("Key differences from the Bayesian approach"),
        tags$table(
            class = "table table-striped",
            style = "max-width:700px;",
            tags$thead(
                tags$tr(tags$th("Feature"), tags$th("ANOVA"), tags$th("Bayesian"))
            ),
            tags$tbody(
                tags$tr(tags$td("Distributional assumptions"),
                        tags$td("All Normal"),
                        tags$td("Normal and/or Student-\\(t\\)")),
                tags$tr(tags$td("Heterogeneous \\(\\sigma_i\\)"),
                        tags$td("Post-hoc per subject"),
                        tags$td("Joint hierarchical model")),
                tags$tr(tags$td("Prior information"),
                        tags$td("None"),
                        tags$td("User-specified informative priors")),
                tags$tr(tags$td("Intervals"),
                        tags$td("Confidence intervals"),
                        tags$td("Credible intervals (full posterior)")),
                tags$tr(tags$td("Prediction for new subject"),
                        tags$td("Not directly available"),
                        tags$td("\\(\\sigma_{I,\\text{pred}}\\) from predictive distribution")),
                tags$tr(tags$td("Robustness to outliers"),
                        tags$td("Sensitive"),
                        tags$td("Naturally handled by heavy-tailed models"))
            )
        )
    )
}

.doc_section_log_transform <- function() {
    .doc_section(
        header = "11. Log-Transformation",
        p("For many analytes the variability scales with the concentration level.
           In such cases a log-transformation stabilises the variance and makes
           the Normal (or Student-\\(t\\)) assumption more appropriate."),
        p("When log-transformation is enabled, the model is fitted to
           \\(y^* = \\ln(y)\\). All model parameters then live on the log scale:"),
        tags$ul(
            tags$li("\\(\\beta\\) becomes the log-mean, \\(\\hat{\\beta}_{\\text{orig}} = \\exp(\\beta)\\)."),
            tags$li("\\(\\sigma_G,\\, \\sigma_I,\\, \\sigma_A\\) are log-scale standard deviations."),
            tags$li("Subject set-points are back-transformed as \\(\\mu_i = \\exp(\\beta + G_i)\\).")
        ),
        p("The CV formulae on the log scale (Section\u00a07.2) exploit the fact that for a
           log-normal random variable \\(Y = \\exp(X)\\) with \\(X \\sim \\mathcal{N}(\\mu, \\sigma)\\):"),
        p(style = "text-align:center;",
          "\\[\\mathrm{CV}(Y) = \\sqrt{\\exp(\\sigma^2) - 1}\\]"),
        p("which depends only on \\(\\sigma\\) and not on \\(\\mu\\). This scale-independence is a
           major advantage of fitting on the log scale."),
        .doc_tip("Choose log-transformation when scatter plots of the raw data show fan-shaped
                  (heteroscedastic) patterns, i.e.\u00a0the spread increases with the concentration.")
    )
}

.doc_section_diagnostics <- function() {
    .doc_section(
        header = "12. MCMC Diagnostics",
        p("The application reports several diagnostics to assess the quality of the
           posterior approximation:"),
        tags$ul(
            tags$li(tags$strong("\\(\\hat{R}\\) (R-hat):"),
                    "Measures convergence across MCMC chains. Values near 1.00 indicate
                     convergence; values above 1.01 are cause for concern."),
            tags$li(tags$strong("Effective Sample Size (ESS):"),
                    "The number of independent draws equivalent to the (autocorrelated)
                     MCMC chain. Low ESS means the posterior is poorly explored. Both",
                    tags$em("bulk"), "and", tags$em("tail"), "ESS are reported."),
            tags$li(tags$strong("Divergent transitions:"),
                    "HMC-specific warnings indicating that the sampler encountered
                     problematic geometry. Divergences can bias the posterior and should be
                     addressed (e.g.\u00a0by increasing", tags$code("adapt_delta"), "or
                     rethinking the model).")
        ),
        .doc_tip("Always inspect trace plots and \\(\\hat{R}\\) before interpreting results.
                  Poor diagnostics do not necessarily mean the model is wrong\u2014they mean
                  the sampler has not explored the posterior adequately.")
    )
}

.doc_section_glossary <- function() {
    .doc_section(
        header = "13. Glossary of Symbols",
        tags$table(
            class = "table table-striped",
            style = "max-width:750px;",
            tags$thead(
                tags$tr(tags$th("Symbol"), tags$th("Description"))
            ),
            tags$tbody(
                tags$tr(tags$td("\\(y_{isr}\\)"), tags$td("Observed measurement for subject \\(i\\), sample \\(s\\), replicate \\(r\\)")),
                tags$tr(tags$td("\\(\\beta\\)"), tags$td("Grand mean (population average concentration)")),
                tags$tr(tags$td("\\(G_i\\)"), tags$td("Between-subject random effect for subject \\(i\\)")),
                tags$tr(tags$td("\\(I_{is}\\)"), tags$td("Within-subject random effect at sampling occasion \\(s\\)")),
                tags$tr(tags$td("\\(A_{isr}\\)"), tags$td("Analytical error on replicate \\(r\\)")),
                tags$tr(tags$td("\\(\\sigma_G\\)"), tags$td("Between-subject standard deviation")),
                tags$tr(tags$td("\\(\\sigma_i\\)"), tags$td("Within-subject SD for subject \\(i\\)")),
                tags$tr(tags$td("\\(\\mu_{\\sigma_I}\\)"), tags$td("Population mean of within-subject SDs")),
                tags$tr(tags$td("\\(\\tau_{\\sigma_I}\\)"), tags$td("Population SD of within-subject SDs")),
                tags$tr(tags$td("\\(\\sigma_A\\)"), tags$td("Analytical standard deviation")),
                tags$tr(tags$td("\\(\\nu_I,\\;\\nu_A\\)"), tags$td("Degrees of freedom for Student-\\(t\\) distributions")),
                tags$tr(tags$td("\\(\\mathrm{CV}_G,\\;\\mathrm{CV}_I,\\;\\mathrm{CV}_A\\)"), tags$td("Coefficients of variation (\\(\\%\\)) for each component")),
                tags$tr(tags$td("\\(\\mathrm{CV}_{I,i}\\)"), tags$td("Subject-specific within-individual CV")),
                tags$tr(tags$td("HBHR"), tags$td("Heterogeneity of Biological Variation Ratio")),
                tags$tr(tags$td("RCV"), tags$td("Reference Change Value")),
                tags$tr(tags$td("\\(\\sigma_{I,\\text{pred}}\\)"), tags$td("Predictive within-subject SD for a new individual")),
                tags$tr(tags$td("\\(w_\\bullet\\)"), tags$td("Weakness multiplier controlling prior diffuseness"))
            )
        )
    )
}


# ── Module UI ─────────────────────────────────────────────────────────────────

#' Documentation Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements for the documentation page
mod_documentation_ui <- function(id) {
    ns <- NS(id)
    tagList(
        htmlDependency(
            name = "glass-help",
            version = "1.0.0",
            src = c(file = system.file("assets",
                                       package = "EstimatingBiologicalVariation")),
            script = "glass_help.js",
            stylesheet = "glass_help.css"
        ),
        glassCard(
            inputId = ns("docs_card"),
            title = "Documentation",
            icon = icon("book"),
            glassRow(
                glassCol(
                    12,
                    div(class = "glass-help-card",
                        .doc_tip(
                            "This page describes the mathematical theory behind the
                             biological variation models. Use the sections below to
                             understand the observation model, prior specification,
                             posterior inference, and derived quantities."
                        ),
                        .doc_section_overview(),
                        .doc_section_data_model(),
                        .doc_section_distributions(),
                        .doc_section_lst(),
                        .doc_section_hierarchical_sigma(),
                        .doc_section_priors(),
                        .doc_section_cv(),
                        .doc_section_rcv(),
                        .doc_section_bayesian_inference(),
                        .doc_section_anova_comparison(),
                        .doc_section_log_transform(),
                        .doc_section_diagnostics(),
                        .doc_section_glossary()
                    )
                )
            )
        )
    )
}

# ── Module Server ─────────────────────────────────────────────────────────────

#' Documentation Module Server
#'
#' @param id Namespace ID for the module
mod_documentation_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Static documentation — no server logic required
    })
}
