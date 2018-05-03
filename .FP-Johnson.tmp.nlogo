; Chloe Johnson and Anna Novak
; Professor Dickerson
; CSCI 0390
; May 14, 2018

; 1. How to get around Massachusetts?
; 2. How to speed up our migration?
; 3.


; interface
; approx-init-pop

; Profiler
; extension, not part of core code
extensions [ profiler ]


globals [
  day
  year
  min-window
  max-window
  distribution-variability
  traveling-patches          ; patches in-boundary?
  daily-death-rate
  offseason-death-rate
]

breed [ boats boat ]


patches-own [
  state
  in-boundary?
  zero-to-four
  five-to-nine
  ten-to-fourteen
  fifteen-to-nineteen
  twenty-to-twenty-four
  twenty-five-to-twenty-nine
  total-fish
  visited?
  coastline?
  coast-num
  closest-coast
]


; Observer context
to setup
  ca
  ;if profile? [profiler:start] ; call the start procedure within the profiler extension
  reset-ticks
  import-pcolors "map2d-01.png"
  init-patches
  import-pcolors "map1.png"
  init-globals
  init-fish ; eventually replace with normal-init-fish
  color-patches
  if profile? [
    profiler:stop
    print profiler:report
    profiler:reset
  ] ; exclusive time doesn't include subprocedure time
end

; Observer context
to move
  ;show(f)
  ifelse day < 170 [ ; 170
    if profile? [profiler:start]
    set day day + 1
    color-patches
    ;let fishies traveling-patches with [fish-on-patch > 0]
    ;show fish-population
    daily-death
    migrate
    ;show fish-population
    if profile? [
      profiler:stop
      print profiler:report
      profiler:reset
    ]
  ][
    set day 0
    set year year + 1
    redistribute-fish
    offseason-death
    age-up

  ]
end


; note: need to deal with mixed-color patches. resampling
; Observer context
to init-patches
  ask patches [
    set zero-to-four 0
    set five-to-nine 0
    set ten-to-fourteen 0
    set fifteen-to-nineteen 0
    set twenty-to-twenty-four 0
    set twenty-five-to-twenty-nine 0
  ]
  set-states
  set-coast
end


; Observer context
to set-states
  ask patches [ set in-boundary? false ]
;  ask patches with [pcolor = 5.6] [set state "land"]
;  ask patches with [pcolor = 97.9] [set state "water"]
;  ask patches with [pcolor = 26.9] [set state "ME"]
;  ask patches with [pcolor = 67.8 ] [set state "NH"]
;  ask patches with [pcolor = 63.2] [set state "RI"]
;  ask patches with [pcolor = 15.7] [set state "MA"]
;  ask patches with [pcolor = 45.6 ] [set state "NY"]
;  ask patches with [pcolor = 114.2] [set state "CT"]
;  ask patches with [pcolor = 15.6] [set state "NJ"]
;  ask patches with [state = "ME" or state = "NH" or state = "RI" or state = "MA" or state = "NY" or state = "CT" or state = "NJ"] [ set in-boundary? true ]
  ask patches with [pcolor = 5.6] [set state "land"]
  ask patches with [pcolor = 108] [set state "water"] ;97.9
  ask patches with [pcolor = 25.2] [set state "ME"] ;26.9
  ask patches with [pcolor = 85.5] [set state "NH"] ;67.8
  ask patches with [pcolor = 55.1] [set state "RI"] ; 63.2
  ask patches with [pcolor = 125.1] [set state "MA"]  ;15.7
  ask patches with [pcolor = 45.2 ] [set state "NY"] ; 45.6
  ask patches with [pcolor = 115] [set state "CT"] ; 114.2
  ask patches with [pcolor = 14.7] [set state "NJ"]  ;15.6
  ask patches with [state = "ME" or state = "NH" or state = "RI" or state = "MA" or state = "NY" or state = "CT" or state = "NJ"] [ set in-boundary? true ]
end


to set-coast

end

; Observer context
to init-globals
  set day 0
  set year 0
  set min-window 0
  set max-window 40
  set distribution-variability 0.2
  set traveling-patches patches with [in-boundary?]
  set daily-death-rate 0.00055
  set offseason-death-rate .107
end


; Observer context
to init-fish
  let npiw num-patches-in-window
  let distribution round (approx-init-pop / npiw)
  ask traveling-patches with [pxcor >= min-window and pxcor <= max-window and pycor < 138] [ ; delete pycor when map is fixed

    let plus-minus random 2
    ifelse plus-minus = 0 [
      set zero-to-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set zero-to-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set five-to-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set five-to-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set ten-to-fourteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set ten-to-fourteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set fifteen-to-nineteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set fifteen-to-nineteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set twenty-to-twenty-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set twenty-to-twenty-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set twenty-five-to-twenty-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set twenty-five-to-twenty-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set total-fish fish-on-patch
  ]
end


; Distribute initial fish population throughout the starting patches (based on a normal distribution)
; Observer context
to normal-init-fish
  let npiw num-patches-in-window
  let distribution floor (approx-init-pop / npiw)

  repeat npiw [

    let patch-to-fill patch random-normal 20 5 random-normal 130 5
    while [[not in-boundary?] of patch-to-fill] [
      set patch-to-fill patch random-normal 20 5 random-normal 130 5
    ]

    ask patch-to-fill [
      let plus-minus random 2
      ifelse plus-minus = 0 [
        set zero-to-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
      ][
        set zero-to-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
      ]
      set plus-minus random 2
    ifelse plus-minus = 0 [
      set five-to-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set five-to-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set ten-to-fourteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set ten-to-fourteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set fifteen-to-nineteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set fifteen-to-nineteen round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set twenty-to-twenty-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set twenty-to-twenty-four round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

    set plus-minus random 2
    ifelse plus-minus = 0 [
      set twenty-five-to-twenty-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability))))
    ][
      set twenty-five-to-twenty-nine round ((0.12 + random-float 0.08) * (distribution + (round (distribution * random-float distribution-variability * -1))))
    ]

      set total-fish fish-on-patch
  ]
  ]

  ; Call migrate once to spread out the normally-distributed fish
  migrate
end


to redistribute-fish
  let piw patches-in-window
  ask traveling-patches with [fish-on-patch > 0][
    ask one-of piw [
      set twenty-five-to-twenty-nine twenty-five-to-twenty-nine + [twenty-five-to-twenty-nine] of myself
      set twenty-to-twenty-four twenty-to-twenty-four + [twenty-to-twenty-four] of myself
      set fifteen-to-nineteen fifteen-to-nineteen + [fifteen-to-nineteen] of myself
      set ten-to-fourteen ten-to-fourteen + [ten-to-fourteen] of myself
      set five-to-nine five-to-nine + [five-to-nine] of myself
      set zero-to-four zero-to-four + [zero-to-four] of myself
    ]
    set zero-to-four 0
    set five-to-nine 0
    set ten-to-fourteen 0
    set fifteen-to-nineteen 0
    set twenty-to-twenty-four 0
    set twenty-five-to-twenty-nine 0
  ]
  ask traveling-patches [
    set total-fish fish-on-patch
  ]
end

; Observer context
to age-up
  ask traveling-patches with [fish-on-patch > 0] [
    ; the thirty year old fish die
    set twenty-five-to-twenty-nine ((floor (twenty-five-to-twenty-nine / 5)) * 4)
    ; the twenty-four year old fish age up to the twenty-five-to-twenty-nine age group
    set twenty-five-to-twenty-nine (twenty-five-to-twenty-nine + ((floor (twenty-to-twenty-four / 5)) * 4))
    set twenty-to-twenty-four ((floor (twenty-to-twenty-four / 5)) * 4)
    ; the nineteen year old fish age up to the twenty-to-twenty-four age group
    set twenty-to-twenty-four (twenty-to-twenty-four + ((floor (fifteen-to-nineteen / 5)) * 4))
    set fifteen-to-nineteen ((floor (fifteen-to-nineteen / 5)) * 4)
    ; the fourteen year old fish age up to the fifteen-to-nineteen age group
    set fifteen-to-nineteen (fifteen-to-nineteen + ((floor (ten-to-fourteen / 5)) * 4))
    set ten-to-fourteen ((floor (ten-to-fourteen / 5)) * 4)
    ; the nine year old fish age up to the ten-to-fourteen age group
    set ten-to-fourteen (ten-to-fourteen + ((floor (five-to-nine / 5)) * 4))
    set five-to-nine ((floor (five-to-nine / 5)) * 4)
    ; the four year old fish age up to the ten-to-fourteen age group
    set five-to-nine (five-to-nine + ((floor (zero-to-four / 5)) * 4))
    set zero-to-four ((floor (zero-to-four / 5)) * 4)
    ; new fish are born into the zero-to-four age group
    spawn
  ]
  ask traveling-patches [
    set total-fish fish-on-patch
  ]
end

;Observer context
to offseason-death
  ask traveling-patches with [total-fish > 0][
    set zero-to-four zero-to-four - (ceiling (offseason-death-rate * zero-to-four))
    set five-to-nine five-to-nine - (ceiling (offseason-death-rate * five-to-nine))
    set ten-to-fourteen ten-to-fourteen - (ceiling (offseason-death-rate * ten-to-fourteen))
    set fifteen-to-nineteen fifteen-to-nineteen - (ceiling (offseason-death-rate * fifteen-to-nineteen))
    set twenty-to-twenty-four twenty-to-twenty-four - (ceiling (offseason-death-rate * twenty-to-twenty-four))
    set twenty-five-to-twenty-nine twenty-five-to-twenty-nine - (ceiling (offseason-death-rate * twenty-five-to-twenty-nine))
  ]
end


; Patch context
to spawn
  set zero-to-four zero-to-four +
  ; the female five-to-nine fish will each spawn approximately 4000 surviving babies
  floor ((five-to-nine / 2) * ((0.75 + random 0.5) * 0.4)) +
  ; the female ten-to-fourteen fish will each spawn approximately 16000 surviving babies
  floor ((ten-to-fourteen / 2) * ((0.75 + random 0.5) * 1.6)) +
  ; the female fifteen-to-nineteen fish will each spawn approximately 24000 surviving babies
  floor ((fifteen-to-nineteen / 2) * ((0.75 + random 0.5) * 2.4)) +
  ; the female twenty-to-twenty-four fish will each spawn approximately 30000 surviving babies
  floor ((twenty-to-twenty-four / 2) * ((0.75 + random 0.5) * 3)) +
  ; the female twenty-five-to-twenty-nine fish will each spawn approximately 34000 surviving babies
  floor ((twenty-five-to-twenty-nine / 2) * ((0.75 + random 0.5) * 3.4))
end


;Observer context
to daily-death
  ask traveling-patches with [total-fish > 0][
    set zero-to-four zero-to-four - (ceiling (daily-death-rate * zero-to-four))
    set five-to-nine five-to-nine - (ceiling (daily-death-rate * five-to-nine))
    set ten-to-fourteen ten-to-fourteen - (ceiling (daily-death-rate * ten-to-fourteen))
    set fifteen-to-nineteen fifteen-to-nineteen - (ceiling (daily-death-rate * fifteen-to-nineteen))
    set twenty-to-twenty-four twenty-to-twenty-four - (ceiling (daily-death-rate * twenty-to-twenty-four))
    set twenty-five-to-twenty-nine twenty-five-to-twenty-nine - (ceiling (daily-death-rate * twenty-five-to-twenty-nine))
  ]
end


; Observer context
to migrate
  ask traveling-patches with [total-fish > 0] [
    let good-neighbors find-good-neighbors
    ask good-neighbors [
      migrate-zero-to-four
      migrate-five-to-nine
      migrate-ten-to-fourteen
      migrate-fifteen-to-nineteen
      migrate-twenty-to-twenty-four
      migrate-twenty-four-to-twenty-nine
      set total-fish fish-on-patch
      ask myself [set total-fish fish-on-patch]
    ]
  ]
end


; Patch context
to migrate-zero-to-four
  let n-zero-to-four (random-float 0.2) * [zero-to-four] of myself
  let frac-zero-to-four n-zero-to-four - (floor n-zero-to-four)
  let floor-zero-to-four floor n-zero-to-four
  let prob random-float 1
  if prob < frac-zero-to-four [ set floor-zero-to-four floor-zero-to-four + 1 ]
  set zero-to-four zero-to-four + floor-zero-to-four
  ask myself [set zero-to-four zero-to-four - floor-zero-to-four]
end


; Patch context
to migrate-five-to-nine
  let n-five-to-nine (random-float 0.2) * [five-to-nine] of myself
  let frac-five-to-nine n-five-to-nine - (floor n-five-to-nine)
  let floor-five-to-nine floor n-five-to-nine
  let prob random-float 1
  if prob < frac-five-to-nine [ set floor-five-to-nine floor-five-to-nine + 1 ]
  set five-to-nine five-to-nine + floor-five-to-nine
  ask myself [set five-to-nine five-to-nine - floor-five-to-nine]
end


; Patch context
to migrate-ten-to-fourteen
  let n-ten-to-fourteen (random-float 0.2) * [ten-to-fourteen] of myself
  let frac-ten-to-fourteen n-ten-to-fourteen - (floor n-ten-to-fourteen)
  let floor-ten-to-fourteen floor n-ten-to-fourteen
  let prob random-float 1
  if prob < frac-ten-to-fourteen [ set floor-ten-to-fourteen floor-ten-to-fourteen + 1 ]
  set ten-to-fourteen ten-to-fourteen + floor-ten-to-fourteen
  ask myself [set ten-to-fourteen ten-to-fourteen - floor-ten-to-fourteen]
end


; Patch context
to migrate-fifteen-to-nineteen
  let n-fifteen-to-nineteen (random-float 0.2) * [fifteen-to-nineteen] of myself
  let frac-fifteen-to-nineteen n-fifteen-to-nineteen - (floor n-fifteen-to-nineteen)
  let floor-fifteen-to-nineteen floor n-fifteen-to-nineteen
  let prob random-float 1
  if prob < frac-fifteen-to-nineteen [ set floor-fifteen-to-nineteen floor-fifteen-to-nineteen + 1 ]
  set fifteen-to-nineteen fifteen-to-nineteen + floor-fifteen-to-nineteen
  ask myself [set fifteen-to-nineteen fifteen-to-nineteen - floor-fifteen-to-nineteen]
end


; Patch context
to migrate-twenty-to-twenty-four
  let n-twenty-to-twenty-four (random-float 0.2) * [twenty-to-twenty-four] of myself
  let frac-twenty-to-twenty-four n-twenty-to-twenty-four - (floor n-twenty-to-twenty-four)
  let floor-twenty-to-twenty-four floor n-twenty-to-twenty-four
  let prob random-float 1
  if prob < frac-twenty-to-twenty-four [ set floor-twenty-to-twenty-four floor-twenty-to-twenty-four + 1 ]
  set twenty-to-twenty-four twenty-to-twenty-four + floor-twenty-to-twenty-four
  ask myself [set twenty-to-twenty-four twenty-to-twenty-four - floor-twenty-to-twenty-four]
end


; Patch context
to migrate-twenty-four-to-twenty-nine
  let n-twenty-five-to-twenty-nine (random-float 0.2) * [twenty-five-to-twenty-nine] of myself
  let frac-twenty-five-to-twenty-nine n-twenty-five-to-twenty-nine - (floor n-twenty-five-to-twenty-nine)
  let floor-twenty-five-to-twenty-nine floor n-twenty-five-to-twenty-nine
  let prob random-float 1
  if prob < frac-twenty-five-to-twenty-nine [ set floor-twenty-five-to-twenty-nine floor-twenty-five-to-twenty-nine + 1 ]
  set twenty-five-to-twenty-nine twenty-five-to-twenty-nine + floor-twenty-five-to-twenty-nine
  ask myself [set twenty-five-to-twenty-nine twenty-five-to-twenty-nine - floor-twenty-five-to-twenty-nine]
end


;pick nearest, not visited neighbor
; list of coastal patches in increasing order
;

; Patch context
to-report find-good-neighbors
  let good-neighbors patches in-radius 8 with
    [(pxcor = [pxcor] of myself or pxcor = [pxcor] of myself + 6 or pxcor = [pxcor] of myself + 8)
      and (pycor = [pycor] of myself
        or pycor = [pycor] of myself + 7
        or pycor = [pycor] of myself - 7
        or pycor = [pycor] of myself + 5
        or pycor = [pycor] of myself - 5)
      and in-boundary?] ;or pycor = [pycor] of myself + 1 or pycor = [pycor] of myself - 1)]
  if not any? good-neighbors [ set good-neighbors patches in-radius 10 with
    [pxcor <= [pxcor] of myself
      and (pycor = [pycor] of myself
        or pycor = [pycor] of myself + 10
        or pycor = [pycor] of myself - 10
      )
      and in-boundary?]
  ]
  report good-neighbors
end


; Observer context
to color-patches
  ask traveling-patches with [fish-on-patch >= 0] [
    if fish-on-patch = 0 [
    set pcolor 97.9
    ]
    if fish-on-patch > 1 and fish-on-patch <= 10 [
    set pcolor 96
    ]
    if fish-on-patch > 10 and fish-on-patch <= 100 [
    set pcolor 106
    ]
    if fish-on-patch > 100 and fish-on-patch <= 1000 [
    set pcolor 116
    ]
    if fish-on-patch > 1000 and fish-on-patch <= 10000 [
    set pcolor 126
    ]
    if fish-on-patch > 100000 [
    set pcolor 16
    ]
  ]
end

; Patch context
to-report fish-on-patch
  let total 0
  set total total +
    zero-to-four +
    five-to-nine +
    ten-to-fourteen +
    fifteen-to-nineteen +
    twenty-to-twenty-four +
    twenty-five-to-twenty-nine
  report total
end


; Observer context
to-report fish-population
  let total 0
  ask traveling-patches [
    set total total +
    zero-to-four +
    five-to-nine +
    ten-to-fourteen +
    fifteen-to-nineteen +
    twenty-to-twenty-four +
    twenty-five-to-twenty-nine
  ]
  report total
end

to-report num-patches-in-window
  report count traveling-patches with [pxcor >= min-window and pxcor <= max-window and pycor < 138] ;remove 138
end

to-report patches-in-window
  report traveling-patches with [pxcor >= min-window and pxcor <= max-window and pycor < 138] ;remove 138
end





; NOTES
; let n = p * x
; let n-frac = n - floor n
; let n floor r
; take an additional fish with probability n-frac


;; Observer context
;to update-window
;  set min-window min-window + 4
;  set max-window max-window + 4
;end


;let taken-twenty-five-to-twenty-nine floor ((random-float 0.2) * [twenty-five-to-twenty-nine] of myself)
; set twenty-five-to-twenty-nine twenty-five-to-twenty-nine + taken-twenty-five-to-twenty-nine
; ask myself [set twenty-five-to-twenty-nine twenty-five-to-twenty-nine - taken-twenty-five-to-twenty-nine]
@#$#@#$#@
GRAPHICS-WINDOW
210
10
1052
311
-1
-1
1.0
1
10
1
1
1
0
0
0
1
0
833
0
291
0
0
1
ticks
30.0

BUTTON
16
102
82
135
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
4
62
200
95
approx-init-pop
approx-init-pop
0
10000000
4200000.0
10000
1
NIL
HORIZONTAL

BUTTON
17
142
82
175
NIL
move
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1067
12
1124
57
NIL
day
17
1
11

MONITOR
1068
65
1178
110
NIL
fish-population
17
1
11

MONITOR
1131
13
1188
58
NIL
year
17
1
11

SWITCH
90
102
193
135
profile?
profile?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
