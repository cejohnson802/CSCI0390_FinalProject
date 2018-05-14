; Chloe Johnson and Anna Novak
; Professor Dickerson
; CSCI 0390
; May 14, 2018

; We have neither given nor received unauthorized aid on this assignment. Chloe Johnson and Anna Novak

__includes [ "migration.nls" "fishing.nls" ]
extensions [ profiler ]


; ------------- BREEDS, GLOBAL & INSTANCE VARIABLES -------------

globals [
  day                                 ; the current day
  year                                ; the current year
  min-window                          ; the minimum initial pxcor for the fish to occupy
  max-window                          ; the maximum initial pxcor for the fish to occupy
  distribution-variability            ; variation in distribution of initial fish population
  traveling-patches                   ; agentset of patches in-boundary?
  fish-patches                        ; list of patches with a total-fish population > 0
  two-day-death-rate                  ; the death rate that all fish experience every two days in-season (i.e. on each tick)
  offseason-death-rate                ; the death rate that all fish experience in total during the off-season
  boat-radius                         ; the radius of patches in which a boat can catch fish
  catch-probability                   ; the probability of catching fish on that patch (used in each age group)
  coastal-patches                     ; the patches that make up the coastline
  migratory-distribution-variability  ; the maximum percentage of fish that can migrate (used in each age group)
  num-neighbors                       ; the number of other patches that a patch can transfer fish to on each tick (number of patches stored in the patches-own variable good-neighbors)
  travel-allowance                    ; the radius used to determine which patches a given patch a can transfer fish to on each tick
  percent-done
]

breed [ boats boat ]                  ; a new breed that can travel through traveling-patches and catch fish

boats-own [
  current-state                       ; the current state through which the boat is traveling
  boat-total-fish                     ; the total number of fish that this boat has ever caught over all iterations
  caught-0to4                         ; the number of fish aged 0-4 that the boat caught on this iteration; resets to 0 after every tick (0 to 23 inches in length)
  caught-5to9                         ; the number of fish aged 4-9 that the boat caught on this iteration; resets to 0 after every tick (24 to 32 inches in length)
  caught-10to14                       ; the number of fish aged 19-14 that the boat caught on this iteration; resets to 0 after every tick (33 to 43 inches in length)
  caught-15to19                       ; the number of fish aged 15-19 that the boat caught on this iteration; resets to 0 after every tick (44 to 53 inches in length)
  caught-20to24                       ; the number of fish aged 20-24 that the boat caught on this iteration; resets to 0 after every tick (54 to 61 inches in length)
  caught-25to29                       ; the number of fish aged 25-29 that the boat caught on this iteration; resets to 0 after every tick (62 to 70 inches in length)
  fish-caught                         ; the total number of fish caught on this iteration; resets to 0 after every tick
]

patches-own [
  state                              ; the state that the patch exists in; "land" or "water" if not a traveling patch, a state code otherwise (e.g. "CT")
  in-boundary?                       ; true if the patch is within the US maritime boundary (water), false otherwise
  zero-to-four                       ; the number of fish on the patch aged 0 to 4
  five-to-nine                       ; the number of fish on the patch aged 5 to 9
  ten-to-fourteen                    ; the number of fish on the patch aged 10 to 14
  fifteen-to-nineteen                ; the number of fish on the patch aged 15 to 19
  twenty-to-twenty-four              ; the number of fish on the patch aged 20 to 24
  twenty-five-to-twenty-nine         ; the number of fish on the patch aged 25 to 29
  total-fish                         ; the total fish on the patch
  visited?                           ; true if patch has been considered in the coastline search or in the fish-patches list, false otherwise
  coastline?                         ; true if patch is a coastline patch, false otherwise (used for migration)
  coast-num                          ; 0 if patch is not a member of the coastline, >0 if patch is on coastline; increases sequentially from NJ to ME
  closest-coast                      ; 0 if patch is not a traveling-patch; otherwise holds the coast-num of the closest coastline patch
  good-neighbors                     ; an agentset of the patches to which this patch can transfer fish for migration
]


; ------------- SETUP & BASIC INITIALIZATION PROCEDURES -------------

; Setup procedure
; Observer context
to setup
  ca
  import-pcolors "map2e.png"
  init-patches
  init-globals
  init-boats
  import-pcolors "map1.png"
  init-fish
  color-patches
  reset-ticks
end


; Initializes patches-own variables and patch-related globals
; Observer context
to init-patches
  set travel-allowance 7
  set num-neighbors 10  ;10
  ask patches [
    set zero-to-four 0
    set five-to-nine 0
    set ten-to-fourteen 0
    set fifteen-to-nineteen 0
    set twenty-to-twenty-four 0
    set twenty-five-to-twenty-nine 0
    set in-boundary? false
    set coastline? false
    set coast-num 0
    set visited? false
  ]
  set-states
  set-coast
  set-closest
  set-good-neighbors
end





; Initializes global variables, excluding those needed for init-patches.
; Observer context
to init-globals
  set day 0
  set year 0
  set min-window 0
  set max-window 40
  set distribution-variability 0.2
  set fish-patches []
  set two-day-death-rate 0.0011
  set offseason-death-rate .107
  set boat-radius 1
  set catch-probability 0.5
  set migratory-distribution-variability 0.5
end


; Initializes boat agents
; Observer context
to init-boats
  create-boats num-boats [
    set shape "boat"
    set size 10
    move-to one-of traveling-patches
    set color yellow
    set current-state [state] of patch-here
    set caught-0to4 0   ; 0 to 23 inches in length
    set caught-5to9 0   ; 24 to 32 inches in length
    set caught-10to14 0 ; 33 to 43 inches in length
    set caught-15to19 0 ; 44 to 53 inches in length
    set caught-20to24 0 ; 54 to 61 inches in length
    set caught-25to29 0 ; 62 to 70 inches in length
    set fish-caught 0
  ]
end


; Distributes initial fish population (set via the APPROX-INIT-FISH interface slider) across an initial window of patches.
; Fish are distributed to patches by age group in a process with three random elements. Each age group on a patch receives between 8-12%
; of the number of total fish in the world divided by the number of patches in the starting window, plus or minus the product of that number
; times a random factor (capped by distribution-variability) that may be positive or negative.
; Patches with fish are then added to the fish-patches list (code adapted from sample-migration.netlogo by Matthew Dickerson)
; Observer context
to init-fish
  let npiw num-patches-in-window
  let distribution round (approx-init-pop / npiw)
  ask traveling-patches with [pxcor >= min-window and pxcor <= max-window] [
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
    set fish-patches lput self fish-patches
  ]

end


; Recolors patches according to the number of total fish being held by a patch
; Observer context
to color-patches
  ask traveling-patches with [total-fish >= 0] [
    if total-fish = 0 [
      set pcolor 97.9
    ]
    if total-fish > 1 and total-fish <= 10 [
      set pcolor 96
    ]
    if total-fish > 10 and total-fish <= 100 [
      set pcolor 106
    ]
    if total-fish > 100 and total-fish <= 1000 [
      set pcolor 116
    ]
    if total-fish > 1000 and total-fish <= 10000 [
      set pcolor 126
    ]
    if total-fish > 10000 and total-fish <= 100000 [
      set pcolor 124
    ]
    if total-fish > 100000 [
      set pcolor 16
    ]
  ]
end


;to color-patches2
;  foreach fish-patches [ p -> ask p [
;    if total-fish = 0 [
;      set pcolor 97.9
;    ]
;    if total-fish > 1 and total-fish <= 10 [
;      set pcolor 96
;    ]
;    if total-fish > 10 and total-fish <= 100 [
;      set pcolor 106
;    ]
;    if total-fish > 100 and total-fish <= 1000 [
;      set pcolor 116
;    ]
;    if total-fish > 1000 and total-fish <= 100000 [
;      set pcolor 126
;    ]
;    if total-fish > 10000 and total-fish <= 100000 [
;      set pcolor 124
;    ]
;    if total-fish > 100000[
;      set pcolor 16
;    ]
;    ]
;  ]
;
;end




; ------------------- PATCH INITIALIZATION PROCEDURES ---------------------


; Assigns all patches a state according to the background image imported in the setup procedure.
; Observer context
to set-states
  ask patches [
    ifelse pcolor > 5 and pcolor < 7 and count neighbors4 with [pcolor > 7 ] < 3 [set state "land"][
      ifelse pcolor >= 107 and pcolor < 109 [set state "water"][
        ifelse pcolor > 25 and pcolor < 37 and not any? neighbors4 with [pcolor = 14.7 or pcolor = 45.2 ][set state "ME"][
          ifelse pcolor > 75 and pcolor < 86 [ set state "NH"][
            ifelse pcolor > 125 and pcolor < 136 and not any? neighbors4 with [pcolor = 14.7][set state "MA"][
              ifelse  pcolor > 55 and pcolor < 57 [set state "RI"][
                ifelse pcolor >= 115 and pcolor < 117 and not any? neighbors4 with [pcolor = 125.1][set state "CT"][
                  ifelse pcolor > 37 and pcolor < 47 [set state "NY"][
                    ifelse pcolor > 14 and pcolor < 17 and not any? neighbors4 with [pcolor = 25.2][set state "NJ"][
                    ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  ]
  ask patches with [state = 0][handle-stateless]
  ask patches with [state = "ME" or state = "NH" or state = "RI" or state = "MA" or state = "NY" or state = "CT" or state = "NJ"] [ set in-boundary? true ]
end


; Handles transitions between colors.
; Patch context
to handle-stateless
    set state [state] of one-of neighbors4 with [state != 0]
end


; Identifies and creates an agent-set of patches that are part of the coastline
; Observer context
to set-coast
  let land-patches patches with [state = "land"]
  ask land-patches [
    ifelse any? neighbors with [in-boundary? = true or state = "water"][
      set coastline? true
    ][
      set coastline? false
    ]
  ]
  remove-singles
  remove-doubles
  remove-singles
  set-default-coast
  remove-doubles
  let max-coast 0
  coast-numbers (patch 0 140) max-coast
  set coastal-patches patches with [coast-num > 0]
end


; Patches involved in migration keep track of their closest coastline patch
; Initializes global variable traveling-patches (all patches capable of holding fish)
; Observer context
to set-closest
  set traveling-patches patches with [in-boundary?]
  ask traveling-patches [
    set closest-coast [coast-num] of (min-one-of coastal-patches [distance myself])
  ]
end


; 7 , 4, 6  took 38 min to reach mid-Maine by day 132
; All patches that can carry fish keep track of an agentset of other patches to which they can transfer their fish (good-neighbors).
; Good-neighbors are patches in-radius travel-allowance that are either directly forward with respect to the coastline, forward-and-ahead or forward-and-below by pxcor and pycor.
; Travel-allowance sets the radius considered in determining good neighbors and is initialized in the init-patches procedure.
; Num-neighbors sets the number of patches stored in the agentset and is initialized in the init-patches procedure.
; Observer context
to set-good-neighbors
  let i 0
  let m count traveling-patches
    ask traveling-patches [
        set i i + 1
        set percent-done i / m
        let temp-good-neighbors traveling-patches in-radius 10 with [                           ; 10
      (closest-coast > [closest-coast] of myself and closest-coast < [closest-coast] of myself + 10) or       ; 10
      ((pxcor = [pxcor] of myself or pxcor = [pxcor] of myself + 10 or pxcor = [pxcor] of myself - 10)        ; 10 , 10
      and (pycor = [pycor] of myself
        or pycor = [pycor] of myself + 7  ; 7
        or pycor = [pycor] of myself - 7  ; 7
        or pycor = [pycor] of myself + 9      ; 9
          or pycor = [pycor] of myself - 9 ))]  ; 9
    let n min (list count temp-good-neighbors num-neighbors)
    set good-neighbors n-of n temp-good-neighbors
  ]
end




; Clean up coastline by removing individual protruding patches
; Observer context
to remove-singles
  ask patches with [coastline?] [
    if count neighbors4 with [coastline?] = 1 [set coastline? false]
  ]
end

; Clean up coastline by removing one of a pair of protruding patches
; Observer context
to remove-doubles
  ask patches with [coastline?][
    ; if you have a neighbor above you, to the right of you, and to the diagonal right, set your own coastline? to false
    if [coastline?] of patch (pxcor) (pycor + 1) and [coastline?] of patch (pxcor + 1) (pycor) and [coastline?] of patch (pxcor + 1) (pycor + 1) [
      set coastline? false
    ]
  ]
end

; Handles special cases in the coastline
; Observer context
to set-default-coast
  ask patch 0 140 [set coastline? true]
  ask patch 1 140 [set coastline? true]
  ask patch 2 140 [set coastline? true]
  ask patch 2 139 [set coastline? true]
  ask patch 2 138 [set coastline? true]
  ask patch 3 138 [set coastline? true]
  ask patch 4 138 [set coastline? true]
  ask patch 5 138 [set coastline? true]
  ask patch 470 144 [set coastline? true]
  ask patch 470 145 [set coastline? true]
  ask patch 470 146 [set coastline? true]
  ask patch 470 147 [set coastline? true]
  ask patch 470 148 [set coastline? true]
  ask patch 471 148 [set coastline? true]
  ask patch 472 148 [set coastline? true]
  ask patch 473 148 [set coastline? true]
  ask patch 474 148 [set coastline? true]
  ask patch 474 147 [set coastline? true]
  ask patch 830 255 [set coastline? true]
  ask patch 831 255 [set coastline? true]
  ask patch 832 255 [set coastline? true]
  ask patch 833 255 [set coastline? true]
  ask patch 309 146 [set coastline? false]
  ask patch 310 147 [set coastline? false]
  ask patch 472 147 [set coastline? false]
  ask patch 830 256 [set coastline? false]
  ask patch 831 256 [set coastline? false]
  ask patch 832 256 [set coastline? false]
  ask patch 833 256 [set coastline? false]
end


; Numbers coastline patches in ascending order from New Jersey to Maine
; Observer context
to coast-numbers [my-patch current-num]
  ask my-patch [
    set coast-num current-num
    set visited? true
    set current-num current-num + 1
    if my-patch != patch 832 255 [
      let next-patch one-of neighbors4 with [ not visited? and coastline? ]
      coast-numbers next-patch current-num
    ]
  ]
end


;-------------------- MOVEMENT PROCEDURES ------------------

; Controls all tick-dependent procedures, and
; Called by interface "move" button
; Observer context
to move
  ifelse day < 170 [
    set day day + 2
    color-patches
    if natural-mortality? [
      two-day-death       ; UNCOMMENT ORIGINAL
    ]
    migrate
    fish-NJ
    fish-NY
    fish-CT
    fish-RI
    fish-MA
    fish-NH
    fish-ME
    move-boats
  ][
    set day 0
    set year year + 1
    redistribute-fish
    offseason-death
    age-up            ; UNCOMMENT ORIGINAL
    color-patches
  ]
  tick
end


; Controls movement of boats: boats can choose any location in-cone 3 180 as their destination.
; Observer context
to move-boats
  ask boats [
    let chosen-location one-of traveling-patches in-cone 3 180
    face chosen-location
    move-to chosen-location
    update-state
  ]
end


; Updates boat agent's local knowledge of its current location with respect to state waters.
; Boat context
to update-state
  set current-state [state] of patch-here
end



; Redistribute fish population across the initial window of patches.
; Called by MOVE procedure at the start of a new season (days reaches 170)
; Observer context
to redistribute-fish
  let piw patches-in-window
  let new-fish-patches []

  foreach fish-patches [ p -> ask p [set visited? false]]

  foreach fish-patches [ p -> ask p [
    ask one-of piw [
      set twenty-five-to-twenty-nine twenty-five-to-twenty-nine + [twenty-five-to-twenty-nine] of myself
      set twenty-to-twenty-four twenty-to-twenty-four + [twenty-to-twenty-four] of myself
      set fifteen-to-nineteen fifteen-to-nineteen + [fifteen-to-nineteen] of myself
      set ten-to-fourteen ten-to-fourteen + [ten-to-fourteen] of myself
      set five-to-nine five-to-nine + [five-to-nine] of myself
      set zero-to-four zero-to-four + [zero-to-four] of myself

      if not visited? [
        set visited? true
        set new-fish-patches fput self new-fish-patches
      ]
    ]
    set zero-to-four 0
    set five-to-nine 0
    set ten-to-fourteen 0
    set fifteen-to-nineteen 0
    set twenty-to-twenty-four 0
    set twenty-five-to-twenty-nine 0
    ]
  ]
  set fish-patches new-fish-patches

end


; --------------------- FISH LIFECYCLE PROCEDURES --------------

; ORIGINAL VERSION
; Updates fish age groups
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


; Reduces fish population in each age group by the offseason-death-rate to account for natural death that occurs between seasons
; Observer context
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


; Adds new fish to the youngest age group based on the number of mature female fish, a set probability, and the number of surviving babies expected
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
  floor ((twenty-to-twenty-four / 2) * ((0.75 + random 0.5) * 3.0)) +
  ; the female twenty-five-to-twenty-nine fish will each spawn approximately 34000 surviving babies
  floor ((twenty-five-to-twenty-nine / 2) * ((0.75 + random 0.5) * 3.4))
end


;
; Original version
; Observer context
to two-day-death
  ask traveling-patches with [total-fish > 0][
    set zero-to-four zero-to-four - (ceiling (two-day-death-rate * zero-to-four))
    set five-to-nine five-to-nine - (ceiling (two-day-death-rate * five-to-nine))
    set ten-to-fourteen ten-to-fourteen - (ceiling (two-day-death-rate * ten-to-fourteen))
    set fifteen-to-nineteen fifteen-to-nineteen - (ceiling (two-day-death-rate * fifteen-to-nineteen))
    set twenty-to-twenty-four twenty-to-twenty-four - (ceiling (two-day-death-rate * twenty-to-twenty-four))
    set twenty-five-to-twenty-nine twenty-five-to-twenty-nine - (ceiling (two-day-death-rate * twenty-five-to-twenty-nine))
  ]
end


;-------------------- REPORTERS ----------------

; Reports total population of fish on this patch
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


; Reports total population of fish on all patches in the world
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


; Reports number of patches in the initial window where fish are located at the start of the season
; Observer context
to-report num-patches-in-window
  report count traveling-patches with [pxcor >= min-window and pxcor <= max-window and pycor < 138] ;remove 138
end


; Reports agentset of the patches in the initial window where fish are located at the start of the season
; Observer context
to-report patches-in-window
  report traveling-patches with [pxcor >= min-window and pxcor <= max-window and pycor < 138] ;remove 138
end
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
15
96
81
129
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
6
50
202
83
approx-init-pop
approx-init-pop
0
10000000
1.0E7
10000
1
NIL
HORIZONTAL

BUTTON
15
136
80
169
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
1189
12
1322
57
NIL
fish-population
17
1
11

MONITOR
1128
12
1185
57
NIL
year
17
1
11

SLIDER
6
11
201
44
num-boats
num-boats
0
250
100.0
1
1
NIL
HORIZONTAL

SLIDER
142
341
275
374
NJ-min
NJ-min
0
70
0.0
1
1
in
HORIZONTAL

TEXTBOX
143
324
255
342
New Jersey regulations
10
0.0
1

PLOT
1066
75
1391
311
Atlantic Striped Bass Population over Time
days
fish population
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"fish" 1.0 0 -8053223 true "" "plot fish-population"

SLIDER
288
341
420
374
NY-min
NY-min
0
70
0.0
1
1
in
HORIZONTAL

SLIDER
431
341
562
374
CT-min
CT-min
0
70
0.0
1
1
in
HORIZONTAL

SLIDER
576
341
706
374
RI-min
RI-min
0
70
0.0
1
1
in
HORIZONTAL

SLIDER
718
341
848
374
MA-min
MA-min
0
70
0.0
1
1
in
HORIZONTAL

SLIDER
862
340
992
373
NH-min
NH-min
0
70
0.0
1
1
in
HORIZONTAL

SLIDER
1006
340
1136
373
ME-min
ME-min
0
70
0.0
1
1
in
HORIZONTAL

TEXTBOX
288
323
395
341
New York regulations
10
0.0
1

TEXTBOX
431
324
550
342
Connecticut regulations
10
0.0
1

TEXTBOX
575
325
696
343
Rhode Island regulations
10
0.0
1

TEXTBOX
718
325
849
343
Massachusetts regulations
10
0.0
1

TEXTBOX
861
324
996
342
New Hampshire regulations
10
0.0
1

TEXTBOX
1008
324
1098
342
Maine regulations
10
0.0
1

SLIDER
141
380
275
413
NJ-num
NJ-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

SLIDER
287
381
419
414
NY-num
NY-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

SLIDER
432
382
562
415
CT-num
CT-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

SLIDER
575
383
705
416
RI-num
RI-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

SLIDER
718
383
848
416
MA-num
MA-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

SLIDER
862
383
993
416
NH-num
NH-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

SLIDER
1006
383
1135
416
ME-num
ME-num
0
10
0.0
2
1
fish/2 days
HORIZONTAL

BUTTON
152
430
261
463
NIL
add-NJ-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
301
431
410
464
NIL
add-NY-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
441
432
551
465
NIL
add-CT-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
587
432
696
465
NIL
add-RI-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
728
433
838
466
NIL
add-MA-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
873
433
984
466
NIL
add-NH-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1014
433
1125
466
NIL
add-ME-boat
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
14
223
188
256
NIL
remove-all-boats
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1327
12
1392
57
boats
count boats
17
1
11

SWITCH
14
181
189
214
natural-mortality?
natural-mortality?
0
1
-1000

MONITOR
28
270
117
315
NIL
percent-done
17
1
11

@#$#@#$#@
Chloe Johnson and Anna Novak
Professor Dickerson
CSCI 0390 Final Project
May 14, 2018

We have neither given nor received unauthorized aid on this assignment. Chloe Johnson and Anna Novak

## WHAT IS IT?

This project simulates the effects of state-level fishing regulations (size limits and catch limits) on the Atlantic Striped Bass population. 

This model is intended as an educational tool to understand the effects of overfishing on Striped Bass populations and the impacts of size limit and catch limit regulatory policies.


## HOW IT WORKS

The model simulates 170 days of the Atlantic Striped Bass' northward migration, beginning off the coast of New Jersey in late spring and ending at southern Maine in early fall. Bass population is divided into six age categories (5-9, 10-14, 15-19, 20-24, 25-29 ) and is a property of the patches. 

The initial population (based on approx-init-pop) is normally distributed off the coast of New Jersey to begin the simulation. The population moves northward towards Southern Maine, always remaining in waters within the U.S. maritime boundary (data from NOAA). Boats are initialized (if num-boats is non-zero) to random locations throughout the maritime boundary waters. 


## HOW TO USE IT

Click the SETUP button to set up an initial fish population and fishing boats. 

The APPROX-INIT-FISH slider controls the initial bass population at the start of the first season. 

The NUM-BOATS slider control the initial number of boats in the model. More boat agents can be added by state at any time, with the add-(state code)-boat buttons. 

Click MOVE to begin the simulation. The bass population moves begins to move northward, and boat agents move and catch fish from the patch they are on. 


***The NJ-MIN slider sets the minimum catch size for fish in New Jersey. The NJ-NUM slider sets the maximum number of catches that a boat in New Jersey can make per day, with a catch meaning *** 





## EXPERIMENTAL ANALYSIS

BehaviorSpace experiment. 

Controls- # of seasons to run the model for, # of boats
Independent: size limits, catch limits
(suggested things for the user to notice while running the model)

## ODD PRINCIPLES

number of fish upon initialization
number of fish caught
movement of fish
number of fish on the patch that a boat is on 
movement of boats
daily death rates



## KNOWN ISSUES AND FUTURE WORK

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)


## CREDITS AND REFERENCES

Migrate procedure and fish-patches list implementation adapted from sample-migrate.netlogo by Matthew Dickerson. 

“Atlantic States Marine Fisheries Commission.” Atlantic States Marine Fisheries Commission, www.asmfc.org/species/atlantic-striped-bass.

Appelman, M., Godwin, C., Laney, W. “2017 Review of the Atlantic States Marine Fisheries Commission Fishery 	Management Plan for Atlantic Striped Bass (Morone saxatalis).” Atlantic Striped Bass Plan Review Team. http://www.asmfc.org/uploads/file/59f0e976sbfmpreview2017.pdf

Bouknight, Walter. “Striper Facts.” Sunrise Fishing, www.sunrisefishing.net/striper-facts.html.

Bradley, Caitlin E., et al. “Juvenile and Adult Striped Bass Mortality and Distribution in an Unrecovered Coastal Population.” North American Journal of Fisheries Management, vol. 38, 2017, doi:10.1080/02755947.2017.1396270.

Diodati, Paul J., and R. Anne Richards. “Mortality of Striped Bass Hooked and Released in Salt Water.” Transactions of the American Fisheries Society, vol. 125, no. 2, 1996, pp. 300–307., doi:10.1577/1548-8659.

“Spawning Stripers.” Stripers 247, Allcoast Media, LLC, www.stripers247.com/Spawning-Stripers.php.

“Striped Bass.” National Oceanic and Atmospheric Administration, NOAA, Mar. 2017, chesapeakebay.noaa.gov/index.php?option=com_content&view=article&id=96&Itemid=123
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

boat
true
4
Polygon -1184463 true true 150 15 105 90 90 135 90 240 105 285 195 285 210 240 210 135 195 90 150 15 150 15

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
<experiments>
  <experiment name="Impact of Fishing Regulations in  NJ" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>move</go>
    <exitCondition>day = 80</exitCondition>
    <metric>fish-population</metric>
    <enumeratedValueSet variable="RI-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="profile?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NJ-num">
      <value value="0"/>
      <value value="2"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NY-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ME-min">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-boats">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CT-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NH-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MA-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="speed-up?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RI-min">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NJ-min">
      <value value="0"/>
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NY-min">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural-mortality?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MA-min">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ME-num">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="approx-init-pop">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="CT-min">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="NH-min">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
