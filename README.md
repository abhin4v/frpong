# Gravity Pong

Gravity Pong is the famous [Pong][pong] game with a twist, written in [ClojureScript][cljs] using the 
[core.async][coreasync] library. It can be played [here][demo] (only works on Google Chrome for now).

Along with the usual Pong ball and the paddles on both sides of the screen, there is also a mass at 
the center of the screen which exerts a gravitational force on the ball causing it to change its path
from a usual straight line to a curve. And the user can control the gravitational force using the 
keyboard to shape the ball's path as they want.

## Details for Programmers
It is written in the [Flow-based Programming][fbprog] paradigm where calculations of ball's position, 
velocity etc are done by separate modular components running in their own threads of computation 
(in this case, core.async go processes). These components are connected using signals (implemented 
using core.async channels) to form a network - like an electronic circuit - to build the complete game.
The signals have values which change over time. [`window.requestAnimationFrame`][raf] is used as a 
clock to generate the time ticks to run the game.

The very [loose coupling][coupling] (data coupling) caused by the modular design allows one to add 
new components in the network without changing other parts of it. In fact, this was first developed 
as a usual Pong game and gravitation was added only as an afterthought, with very minimal changes
to the existing code.

Here is how the network of components interconnected with signals looks like:

<pre>
                                      +-d-----------+--------------------------+
                                      v             |                          |
 keyboard +-e-> sampler +---k---> paddle-postnr +-d-+   +-> gravitation +--+   |
                  ^                                 |   p                  a   |
                  t                                 |   |                  |   |
                  |                                 |   |                  |   |
                  +-------+          +-p----------+-|---+                  |   d
                          |          |  +-a-------|-|------+---------------+   |
                          |          v  v         | |      |                   v
  browser +-b--> ticker +-+--t--> ball-postnr +-p-+-|------|------------p-> renderer
                    ^     |             ^         | |      |                  ^  ^
                    |     |       +-----|---------+ |      |                  |  |
                    s     |       |     l     +--d--+      |                  s  t
                    |     |       |   +-+-----|-------+    |                  |  |
                    |     |       p   l   +-a-|-------|----+                  |  |
                    |     |       v   v   v   v  +-l--+                       |  |
                    |     +---t-> collision-detr                              |  |
                    |     |             ^        +-s--+-----------------------+  |
                    |     |             s             |                          |
                    +-----|-------------+-------------+                          |
                          |                                                      |
                          +------------------------------------------------------+
</pre>

Signals     

 - *b*: ticks from browser
 - *t*: game ticks        
 - *e*: keyboard events   
 - *k*: keydowns          
 - *p*: ball position     
 - *l*: ball velocity     
 - *a*: ball acceleration 
 - *d*: paddle positions  
 - *s*: game state        

Components

 - *keyboard*: keyboard for user input
 - *browser*: browser as the primary source of ticks
 - *ticker*: generates game ticks from browser ticks
 - *sampler*: samples input signal based on a sampling signal
 - *paddle-postnr*: paddle positioner, calculates the position of paddles
 - *ball-postnr*: ball positioner, calculates the position of the ball
 - *collision-detr*: collision detector, detects collision of the ball with the paddles and the 
                       walls, also calculates the velocity of the ball
 - *gravitation*: calculates the acceleration of the ball due to gravitation
 - *renderer*: renders the game on screen

This project has been written as an experiment to play with the core.async library and can serve as
a medium sized example project for the same. The main source can be found [here][src].

[pong]: https://en.wikipedia.org/wiki/Pong
[cljs]: https://github.com/clojure/clojurescript
[coreasync]: https://github.com/clojure/core.async
[demo]: http://abhinavsarkar.net/frpong
[fbprog]: https://en.wikipedia.org/wiki/Flow-based_programming
[coupling]: https://en.wikipedia.org/wiki/Coupling_(computer_programming)
[src]: https://github.com/abhin4v/frpong/blob/master/src/cljs/frpong/core.cljs
[raf]: https://developer.mozilla.org/en-US/docs/Web/API/window.requestAnimationFrame
