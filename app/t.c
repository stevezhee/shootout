// clang t.c -lcygwin -lSDL2main -lSDL2

#include <stdio.h>
#include <SDL2/SDL.h>

int main(int argc, char *argv[]) {
    SDL_Window *win = NULL;
    SDL_Renderer *renderer = NULL;
    SDL_Texture *bitmapTex = NULL;
    SDL_Surface *bitmapSurface = NULL;
    /* SDL_Surface *tempSurface = NULL; */
    SDL_Surface *screenSurface = NULL;
    int posX = 100, posY = 100, width = 640, height = 480;

    SDL_Init(SDL_INIT_VIDEO);

    win = SDL_CreateWindow("Hello World", posX, posY, width, height, 0);

    screenSurface = SDL_GetWindowSurface(win);
    if(!screenSurface)
      {
 	printf("%s\n", SDL_GetError());
 	goto cleanup3;
      }
    renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED);
    if(!renderer)
      {
    	printf("%s\n", SDL_GetError());
    	goto cleanup2;
      }
    SDL_SetRenderDrawColor( renderer, 0xFF, 0xFF, 0xFF, 0xFF );

    bitmapSurface = SDL_LoadBMP("ship.bmp");
    if(!bitmapSurface)
      {
 	printf("%s\n", SDL_GetError());
 	goto cleanup;
      }
    /* bitmapSurface = SDL_ConvertSurface(tempSurface, screenSurface->format, 0); */
    /* SDL_FreeSurface(tempSurface); */
    /* if(!bitmapSurface) */
    /*   { */
    /* 	printf("%s\n", SDL_GetError()); */
    /* 	goto cleanup; */
    /*   } */
    bitmapTex = SDL_CreateTextureFromSurface(renderer, bitmapSurface);
    SDL_Rect rect;
    rect.x = 10;
    rect.y = 20;
    rect.w = bitmapSurface->w;
    rect.h = bitmapSurface->h;
    if(!bitmapTex)
      {
    	printf("%s\n", SDL_GetError());
    	goto cleanup;
      }
       SDL_FreeSurface(bitmapSurface);

       int dx;
       int dy;

    while (1) {
        SDL_Event e;
        if (SDL_PollEvent(&e)) {
            if (e.type == SDL_QUIT) {
                break;
            } else {
 	      if (e.type == SDL_KEYDOWN)
 		{
                        switch( e.key.keysym.sym )
                        {
                            case SDLK_UP:
 			      dx = 0;
 			      dy = -1;
                            break;

                            case SDLK_DOWN:
 			      dx = 0;
 			      dy = 1;
                            break;

                            case SDLK_LEFT:
 			      dx = -1;
 			      dy = 0;
                            break;

                            case SDLK_RIGHT:
 			      dx = 1;
 			      dy = 0;
                            break;

                            default:
 			      dx = 0;
 			      dy = 0;
                            break;
                        }		}
 	    }
        }

 	/* SDL_BlitSurface(bitmapSurface, NULL, screenSurface, NULL); */
 	/* SDL_UpdateWindowSurface(win); */
 	rect.x += dx;
 	rect.y += dy;

 	rect.x = rect.x > (width - rect.w) ? (width - rect.w) : rect.x;
 	rect.x = rect.x < 0 ? 0 : rect.x;
 	rect.y = rect.y > (height - rect.h) ? (height - rect.h) : rect.y;
 	rect.y = rect.y < 0 ? 0 : rect.y;

        SDL_RenderClear(renderer);
        SDL_RenderCopy(renderer, bitmapTex, NULL, &rect);
        SDL_RenderPresent(renderer);
    }
 /* cleanup0: */
    SDL_DestroyTexture(bitmapTex);
 cleanup:
    SDL_DestroyRenderer(renderer);
 cleanup2:
    SDL_DestroyWindow(win);
 cleanup3:
    SDL_Quit();

    return 0;
}
