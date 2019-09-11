
# MDN animation reference: https://developer.mozilla.org/en-US/docs/Web/CSS/animation

css_anim_defaults <- list(
  name            = 'none',
  duration        = "0s",
  timing_function = 'ease',
  delay           = "0s",
  iteration_count = 1,
  direction       = 'normal',
  fill_mode       = 'none',
  play_state      = 'running'
)



css_prop$animation <- function(
  name            = 'none',
  duration        = 10,
  timing_function = c('ease', 'ease-in', 'ease-out', 'ease-in-out',
                      'linear', 'step-start', 'step-end'),
  delay           = 0,
  iteration_count = 'infinite',
  direction       = c('normal', 'reverse', 'alternate', 'alternate-reverse'),
  fill_mode       = c('none', 'forwards', 'backwards', 'both'),
  play_state      = c('running', 'paused')
) {

  # Specify duration and delay in seconds
  if (is.numeric(duration) || !endsWith(duration, 's') || !endsWith(duration, 'S')) {
    duration <- paste0(duration, 's')
  }

  if (is.numeric(delay) || !endsWith(delay, 's') || !endsWith(delay, 'S')) {
    delay <- paste0(delay, 's')
  }

  # Assemble the list of all options
  this_anim <- list(
    name            = name,
    duration        = duration,
    timing_function = match.arg(timing_function),
    delay           = delay,
    iteration_count = iteration_count,
    direction       = match.arg(direction),
    fill_mode       = match.arg(fill_mode),
    play_state      = match.arg(play_state)
  )

  # Keep only those options which are different from the CSS default standard
  nn         <- names(css_anim_defaults)
  is_default <- vapply(nn, function(x) {identical(css_anim_defaults[[x]], this_anim[[x]])}, logical(1))
  this_anim  <- this_anim[nn[!is_default]]

  # Create and return the animation property
  opts <- c(
    this_anim$name,
    this_anim$duration,
    this_anim$timing_function,
    this_anim$delay,
    this_anim$iteration_count,
    this_anim$direction,
    this_anim$fill_mode,
    this_anim$play_state
  )

  res <- paste(opts, collapse = " ")
  res <- setNames(list(res), 'animation')
  class(res) <- 'css_property'

  res
}


