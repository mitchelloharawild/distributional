library(distributional)

out_file <- "hex/distributional.svg"

# ---- Canvas and palette (Fletch) ---------------------------------------------

W <- 520; H <- 600
CX <- 260; CY <- 300; R <- 292

pal <- list(
  bg = "#1c2a3d", bg2 = "#213249", ink = "#f2ead9", bow = "#e7ae47",
  bowhi = "#f8d995", tail = "#e05a4f", area = "#2c405b", wood = "#dcc6a2",
  strap = "#47301f", quiver = "#8a5a3b"
)

darken <- function(hex, k = .72) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf(
    "#%02x%02x%02x", 
    as.integer(rgb[1] * k), as.integer(rgb[2] * k), as.integer(rgb[3] * k)
  )
}
arrow_colour <- function(hex) c(hex, darken(hex))

# ---- SVG helpers -------------------------------------------------------------

fmt <- function(v) {
  s <- sprintf("%.1f", v)
  sub("\\.0$", "", s)
}
pts <- function(x, y) paste0(fmt(x), ",", fmt(y), collapse = " ")
svg_path <- function(x, y, close = FALSE) {
  paste0("M", paste0(fmt(x), ",", fmt(y), collapse = " L"), if (close) " Z" else "")
}
group <- function(content, x = 0, y = 0, rot = 0) {
  sprintf(
    '<g transform="translate(%s %s) rotate(%s)">%s</g>', 
    fmt(x), fmt(y), fmt(rot), content
  )
}
hexagon <- function(r = R) {
  a <- seq(0, 300, by = 60) * pi / 180
  list(x = CX + r * sin(a), y = CY - r * cos(a))
}

# Unique ids for clip paths within one sticker
ids <- new.env()
ids$uid <- "rising"; ids$n <- 0
nid <- function(tag) {
  ids$n <- ids$n + 1
  paste0(ids$uid, "-", tag, ids$n)
}

# Offset a polyline along its left normal by d (scalar or per-point)
offset <- function(x, y, d) {
  n <- length(x); d <- rep_len(d, n)
  i0 <- pmax(seq_len(n) - 1, 1); i1 <- pmin(seq_len(n) + 1, n)
  dx <- x[i1] - x[i0]; dy <- y[i1] - y[i0]
  len <- sqrt(dx^2 + dy^2); len[len == 0] <- 1
  list(x = x - dy / len * d, y = y + dx / len * d)
}

# ---- Arrow shapes from distributions ------------------------------------------------

# A continuous distribution drawn over [a, b]: CDF for the fletching, density for the head.
# The head is centred on the shaft by its visible extent (density >= 5% of the peak).
continuous_shape <- function(dist, a, b, n = 160, centre = c("visible", "mode")) {
  centre <- match.arg(centre)
  x <- a + (b - a) * (0:n) / n
  fx <- unlist(density(dist, x))
  Fx <- unlist(cdf(dist, x))
  vis <- x[fx >= .05 * max(fx)]
  ctr <- if (centre == "mode") x[which.max(fx)] else (min(vis) + max(vis)) / 2
  half <- max(ctr - a, b - ctr)
  list(
    cdf_t = (0:n) / n, cdf_F = (Fx - Fx[1]) / (Fx[n + 1] - Fx[1]),
    head_h = fx / max(fx), head_lat = (x - ctr) / half
  )
}

# A discrete distribution on 0..K: stepped fletching and a histogram head
discrete_shape <- function(dist, K) {
  k <- 0:K
  # Truncated to 0..K and renormalised
  p <- unlist(density(dist, k)); p <- p / sum(p)
  cum <- unlist(cdf(dist, k)) / unlist(cdf(dist, K))
  t <- (k + .5) / (K + 1)
  cdf_t <- c(0, rbind(t, t), 1)
  cdf_F <- c(0, rbind(c(0, head(cum, -1)), cum), 1)
  vis <- k[p >= .05 * max(p)]
  ctr <- (min(vis) + max(vis)) / 2
  half <- max(ctr + .5, K + .5 - ctr)
  h <- p / max(p)
  list(
    cdf_t = cdf_t, cdf_F = cdf_F,
    head_h = c(0, rbind(h, h), 0),
    head_lat = c(-.5 - ctr, rbind(k - .5 - ctr, k + .5 - ctr), K + .5 - ctr) / half
  )
}

# Every mode of the mixture must be a real peak, with the centre one tallest
check_trimodal <- function(dist, a, b, n = 300) {
  x <- a + (b - a) * (0:n) / n
  fx <- unlist(density(dist, x))
  i <- which(diff(sign(diff(fx))) < 0) + 1
  stopifnot(length(i) == 3, fx[i[2]] > fx[i[1]], fx[i[2]] > fx[i[3]])
  invisible(fx[i] / fx[i[2]])
}

# ---- Bow: the N(0, 1) density -------------------------------------------------------

bow <- function(span, depth, zmax = 3.2, tmax = 15, tmin = 4.5) {
  n <- 160
  z <- -zmax + 2 * zmax * (0:n) / n
  phi0 <- unlist(density(dist_normal(), 0))
  yz <- function(z) z / zmax * span / 2
  xz <- function(z) depth * unlist(density(dist_normal(), z)) / phi0
  cx <- xz(z); cy <- yz(z)
  out <- sprintf('<path d="%s" fill="%s"/>',
                 svg_path(c(0, cx, 0), c(-span / 2, cy, span / 2), TRUE), pal$area)
  # Shaded tails: the two-sided 5% beyond the 97.5% quantile
  q <- quantile(dist_normal(), .975)
  for (lim in list(c(-zmax, -q), c(q, zmax))) {
    tz <- lim[1] + (lim[2] - lim[1]) * (0:40) / 40
    out <- c(out, sprintf('<path d="%s" fill="%s"/>',
                          svg_path(c(0, xz(tz), 0), c(yz(lim[1]), yz(tz), yz(lim[2])), TRUE), pal$tail))
  }
  # Axis ticks at z = -3..3 on the string side
  for (tk in -3:3) {
    out <- c(out, sprintf('<path d="M-3,%s L-10,%s" stroke="%s" stroke-width="2.2" stroke-linecap="round" opacity=".7"/>',
                          fmt(yz(tk)), fmt(yz(tk)), pal$ink))
  }
  # String (the zero line) and its centre serving
  out <- c(out,
    sprintf('<path d="M0,%s L0,%s" stroke="%s" stroke-width="2.6" stroke-linecap="round"/>', fmt(-span / 2), fmt(span / 2), pal$ink),
    sprintf('<path d="M0,-24 L0,24" stroke="%s" stroke-width="5" stroke-linecap="round"/>', pal$ink))
  # Limb, tapering from the grip to the tips
  th <- tmin + (tmax - tmin) * (xz(z) / depth)^.7
  outer <- offset(cx, cy, th / 2); inner <- offset(cx, cy, -th / 2)
  out <- c(out, sprintf('<path d="%s" fill="%s" stroke="%s" stroke-width="1.5" stroke-linejoin="round"/>',
                        svg_path(c(outer$x, rev(inner$x)), c(outer$y, rev(inner$y)), TRUE), pal$bow, pal$bow))
  for (i in c(1, n + 1)) {
    out <- c(out, sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"/>', fmt(cx[i]), fmt(cy[i]), fmt(tmin / 2 + .8), pal$bow))
  }
  hi <- 23:(n + 1 - 22)
  hl <- offset(cx[hi], cy[hi], -th[hi] * .22)
  out <- c(out, sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="2.4" stroke-linecap="round"/>',
                        svg_path(hl$x, hl$y), pal$bowhi))
  c(out, grip(cx, cy, z, th, tmax))
}

# Leather grip around the mode, wrapped square to the limb and mirrored about the mode
grip <- function(cx, cy, z, th, tmax, z0 = -.42, z1 = .42, step = 9) {
  band <- which(z >= z0 & z <= z1)
  o1 <- offset(cx[band], cy[band], th[band] / 2 + 3)
  o2 <- offset(cx[band], cy[band], -(th[band] / 2 + 3))
  px <- c(o1$x, rev(o2$x)); py <- c(o1$y, rev(o2$y))
  mid <- which.min(abs(z))
  # Walk from the mode by arc length, placing a wrap every `step` px
  walk <- function(direction) {
    marks <- list(); acc <- 0; target <- 0; k <- mid
    while (k + direction >= 1 && k + direction <= length(z) &&
           z[k + direction] >= z0 && z[k + direction] <= z1) {
      ax <- cx[k]; ay <- cy[k]; bx <- cx[k + direction]; by <- cy[k + direction]
      seg <- sqrt((bx - ax)^2 + (by - ay)^2)
      while (acc + seg >= target) {
        t <- (target - acc) / seg
        tx <- (bx - ax) / seg * direction; ty <- (by - ay) / seg * direction
        marks[[length(marks) + 1]] <- c(ax + t * (bx - ax), ay + t * (by - ay), -ty, tx)
        target <- target + step
      }
      acc <- acc + seg; k <- k + direction
    }
    marks
  }
  marks <- c(walk(1), walk(-1)[-1])
  lines <- vapply(marks, function(m) {
    sprintf('<path d="M%s,%s L%s,%s" stroke="%s" stroke-width="3"/>',
            fmt(m[1] - m[3] * tmax), fmt(m[2] - m[4] * tmax), fmt(m[1] + m[3] * tmax), fmt(m[2] + m[4] * tmax), pal$quiver)
  }, character(1))
  id <- nid("grip")
  sprintf(paste0('<clipPath id="%s"><path d="%s"/></clipPath>',
                 '<path d="%s" fill="%s" stroke="%s" stroke-width="2" stroke-linejoin="round"/>',
                 '<g clip-path="url(#%s)">%s</g>'),
          id, svg_path(px, py, TRUE), svg_path(px, py, TRUE), pal$strap, pal$strap, id, paste(lines, collapse = ""))
}

# ---- Arrow: CDF fletching, density head ---------------------------------------------

arrow <- function(L, shape, col, vl = 78, h = 20, shaft = 5.5, hl = 42, hw = 21) {
  c1 <- col[1]; c2 <- col[2]
  out <- sprintf('<path d="M2,0 L%s,0" stroke="%s" stroke-width="%s" stroke-linecap="round"/>', fmt(L - hl + 2), pal$wood, shaft)
  ub <- 9
  for (side in c(-1, 1)) {
    fill <- if (side < 0) c1 else c2
    vx <- c(ub + vl, ub + vl * (1 - shape$cdf_t), ub, ub)
    vy <- c(0, side * h * shape$cdf_F, side * h, 0)
    id <- nid("v")
    u <- ub + 14 + 11 * (0:19); u <- u[u < ub + vl]
    barbs <- sprintf('<path d="M%s,0 L%s,%s" stroke="%s" stroke-width="1.3" opacity=".35"/>',
                     fmt(u), fmt(u - h * .55), fmt(side * h * 1.1), pal$bg)
    out <- c(out,
      sprintf('<clipPath id="%s"><path d="%s"/></clipPath>', id, svg_path(vx, vy, TRUE)),
      sprintf('<path d="%s" fill="%s" stroke="%s" stroke-width="3" stroke-linejoin="round"/>', svg_path(vx, vy, TRUE), fill, fill),
      sprintf('<g clip-path="url(#%s)">%s</g>', id, paste(barbs, collapse = "")))
  }
  out <- c(out,
    sprintf('<path d="M%s,0 L%s,0" stroke="%s" stroke-width="2.4" stroke-linecap="round"/>', fmt(ub - 2), fmt(ub + vl + 2), pal$wood),
    sprintf('<path d="M-4,-5 L3,-5 L3,5 L-4,5" fill="%s" stroke="%s" stroke-width="2" stroke-linejoin="round"/>', c1, c1))
  # Head: the density standing on its base, two-toned to match the fletching
  ox <- L - hl + hl * shape$head_h; oy <- shape$head_lat * hw
  hx <- c(L - hl, ox, L - hl); hy <- c(oy[1], oy, oy[length(oy)])
  d <- svg_path(hx, hy, TRUE)
  id <- nid("h")
  c(out,
    sprintf('<path d="%s" fill="%s" stroke="%s" stroke-width="7" stroke-linejoin="round"/>', d, pal$bg, pal$bg),
    sprintf('<path d="%s" fill="%s" stroke="%s" stroke-width="2" stroke-linejoin="round"/>', d, c2, c2),
    sprintf('<clipPath id="%s"><path d="%s"/></clipPath>', id, d),
    sprintf('<g clip-path="url(#%s)"><rect x="%s" y="%s" width="%s" height="%s" fill="%s"/></g>',
            id, fmt(L - hl - 2), fmt(-hw - 4), fmt(hl + 6), fmt(hw + 4), c1),
    sprintf('<path d="M%s,%s L%s,%s" stroke="%s" stroke-width="4" stroke-linecap="round"/>',
            fmt(L - hl - 1), fmt(-hw * .55), fmt(L - hl - 1), fmt(hw * .55), pal$wood))
}

# ---- Wordmark along the lower-right edge ---------------------------------------------

wordmark <- function(size = 48, inset = 72) {
  hx <- hexagon()
  mx <- (hx$x[3] + hx$x[4]) / 2; my <- (hx$y[3] + hx$y[4]) / 2
  nx <- CX - mx; ny <- CY - my; nl <- sqrt(nx^2 + ny^2)
  ang <- atan2(hx$y[3] - hx$y[4], hx$x[3] - hx$x[4]) * 180 / pi
  # 6.753 em: advance width of "distributional" in Lexend SemiBold
  sprintf(paste0('<text transform="translate(%s %s) rotate(%s)" text-anchor="middle" ',
                 'font-family="Lexend, \'Helvetica Neue\', Arial, sans-serif" font-weight="600" font-size="%s" ',
                 'textLength="%s" lengthAdjust="spacingAndGlyphs" fill="%s">distributional</text>'),
          fmt(mx + nx / nl * inset), fmt(my + ny / nl * inset), fmt(ang), size, fmt(6.753 * size), pal$ink)
}

# ---- The three distributions -------------------------------------------------------------

poisson <- dist_poisson(2.5)                                     # top: six values, mode at the third
normal <- dist_normal(0, 1)                                      # middle
mixture <- dist_mixture(dist_normal(-2.2, .8), dist_normal(.9, .7), dist_normal(3.6, 1.15),
                        weights = c(.28, .37, .35))              # bottom
mix_a <- min(-2.2 - 3.2 * .8, .9 - 3.2 * .7, 3.6 - 3.2 * 1.15)
mix_b <- max(-2.2 + 3.2 * .8, .9 + 3.2 * .7, 3.6 + 3.2 * 1.15)
print(round(check_trimodal(mixture, mix_a, mix_b), 2))

shapes <- list(
  discrete_shape(poisson, K = 5),
  continuous_shape(normal, -3.2, 3.2, centre = "mode"),
  continuous_shape(mixture, mix_a, mix_b, n = 300)
)
colours <- list(arrow_colour("#e05a4f"), arrow_colour("#b6d46a"), arrow_colour("#4cb5a8"))

# ---- Layout -------------------------------------------------------------------------------

lay <- list(bx = 122, by = 318, rot = -30, span = 350, depth = 112, tmax = 18,
            spread = 64, gaps = c(6, 36, 6), L = 192, scale = 1.08)

th <- lay$rot * pi / 180
d <- c(cos(th), sin(th)); p <- c(-sin(th), cos(th))
belly <- c(lay$bx, lay$by) + lay$depth * d
offsets <- c(-lay$spread, 0, lay$spread)
nocks <- lapply(1:3, function(i) belly + lay$gaps[i] * d + offsets[i] * p)
s <- lay$scale; L <- lay$L * s

# Target rings centred on the Normal arrow's head
ring_c <- nocks[[2]] + (L - 21 * s) * d
rings <- sprintf('<circle cx="%s" cy="%s" r="%s" fill="%s"/>', fmt(ring_c[1]), fmt(ring_c[2]),
                 c(214, 164, 114, 64), rep(c(pal$bg2, pal$bg), 2))

body <- c(rings, group(paste(bow(lay$span, lay$depth, tmax = lay$tmax), collapse = ""), lay$bx, lay$by, lay$rot))
for (i in 1:3) {
  a <- arrow(L, shapes[[i]], colours[[i]], vl = 78 * s, h = 20 * s, shaft = 5.5 * s, hl = 42 * s, hw = 21 * s)
  body <- c(body, group(paste(a, collapse = ""), nocks[[i]][1], nocks[[i]][2], lay$rot))
}
body <- c(body, wordmark())

# ---- Sticker ---------------------------------------------------------------------------------

hx <- hexagon(); hx_in <- hexagon(R - 13)
svg <- paste0(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s %s" role="img" aria-label="distributional hex sticker">', W, H),
  sprintf('<defs><clipPath id="clip-rising"><polygon points="%s"/></clipPath></defs>', pts(hx$x, hx$y)),
  sprintf('<polygon points="%s" fill="%s"/>', pts(hx$x, hx$y), pal$bg),
  sprintf('<g clip-path="url(#clip-rising)">%s</g>', paste(body, collapse = "")),
  sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="2" stroke-dasharray="2 7" stroke-linecap="round" opacity=".5"/>',
          pts(hx_in$x, hx_in$y), pal$ink),
  sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="12" stroke-linejoin="round"/></svg>', pts(hx$x, hx$y), pal$ink)
)
writeLines(svg, out_file)
