# solving SDL2 lessons  

   Problems in [here](http://lazyfoo.net/tutorials/SDL/).  
   With great help by [this repository](https://github.com/palf/haskell-sdl2-examples).  

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
         | no | row size | stretched, slow |  
         | yes | (needless) | stretched, fast |  

         maybe.  

