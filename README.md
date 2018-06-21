# solving SDL2 lessons  

   Problems in [here](http://lazyfoo.net/tutorials/SDL/).  
   With great help by [thi repository](https://github.com/haskell-game/sdl2/blob/master/examples/lazyfoo) or [this one](https://github.com/palf/haskell-sdl2-examples).  

# about this repository  

   Surely, the library `{lesson dir}/Src/SdlUtils*.hs` in each lesson gets better step by step.  
   `lesson 01 'is the most noobish, and the latest one is the best :)  

# policy  

   I solve lessons with haskell.  
   Maybe.  

# memo  

   *  lesson01  
      +  I implemented it very simplely.  

   *  lesson02  
      +  I split some modules into function.  

   *  lesson03  
      +  I separate some modules as SDL2Utils.  

   *  lesson04  
      +  I made effection by tapping direction key.  
      +  TODO : separate key control modules.  

   *  lesson05  
      +  `SDL.surfaceBlit` is different from `SDL.surfaceBlitScaled`.  

         | with Format | Blit | BlitScaled |  
         |:-:|:-:|:-:|  
         | no | raw size | stretched, slow |  
         | yes | (needless) | stretched, fast |  

   *  lesson06  
      +  skip this lesson and recover in l07.  

   *  lesson07  
      +  I use render and texture.  

   *  lesson08  
      +  `SDL.clear` means *Fill randerer with color which you choose*.  

   *  lesson09  
      +  `SDL.RenderScaleQuality` is hint of scaling quality.  
         1. ScaleNearest : Nearest pixel sampling (default)  
         1. ScaleLinear : linear filtering (supported by OpenGL and Direct3D)  
         1. ScaleBest : Anisotropic filtering (supported by Direct3D)  

      +  `viewport`  
         -  Initial viewport is full area of canvas.  
            If change vp in loop, initial vp of 2nd loop has remained changed.  
            So, we should set vp first in loop.  

   *  lesson10  
      +  `XLGBadRenderRequest`  
         -  When I use 8bit and 32bit png instead of 24bit, I got this ERROR at `SDL.copy`.  
         -  Both 8bit and 32bit png have alpha channel.  

