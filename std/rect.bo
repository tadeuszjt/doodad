module rect

import vec2
import strings

type Rect (min Vec2, max Vec2)

fn string(r Rect)
    let s = ""
    s <- "Rect(" <- string(r.min) <- ", " <- string(r.max) <- ")"
    return s

fn area(r Rect)
    return (r.max.x - r.min.x) * (r.max.y - r.min.y)
