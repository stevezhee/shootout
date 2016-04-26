// clang t.c -lcygwin -lSDL2main -lSDL2
// clang -std=c++11 -S -emit-llvm

#include <stdio.h>
#include <SDL2/SDL.h>
SDL_Rect _rec_SDL_Rect_rec_;
auto _fld_r_x = &_rec_SDL_Rect_rec_.x;
auto _fld_r_y = &_rec_SDL_Rect_rec_.y;
auto _fld_r_w = &_rec_SDL_Rect_rec_.w;
auto _fld_r_h = &_rec_SDL_Rect_rec_.h;

SDL_Surface _rec_SDL_Surface_rec_;
auto _fld_s_w = &_rec_SDL_Surface_rec_.w;
auto _fld_s_h = &_rec_SDL_Surface_rec_.h;

SDL_Event _rec_SDL_Event_rec_;
auto _fld_e_type = &_rec_SDL_Event_rec_.type;
auto _fld_e_key = &_rec_SDL_Event_rec_.key;

SDL_KeyboardEvent _rec_SDL_KeyboardEvent_rec_;
auto _fld_ke_keysym = &_rec_SDL_KeyboardEvent_rec_.keysym;

SDL_Keysym _rec_SDL_Keysym_rec_;
auto _fld_ks_sym = &_rec_SDL_Keysym_rec_.sym;

auto _fun_SDL_Init = SDL_Init;
auto _fun_SDL_CreateWindow = SDL_CreateWindow;
auto _fun_SDL_DestroyTexture = SDL_DestroyTexture;
auto _fun_SDL_DestroyRenderer = SDL_DestroyRenderer;
auto _fun_SDL_DestroyWindow = SDL_DestroyWindow;
auto _fun_SDL_Quit = SDL_Quit;
auto _fun_SDL_GetWindowSurface = SDL_GetWindowSurface;
auto _fun_SDL_GetError = SDL_GetError;
auto _fun_SDL_CreateRenderer = SDL_CreateRenderer;
auto _fun_SDL_SetRenderDrawColor = SDL_SetRenderDrawColor;
auto _fun_SDL_LoadBMP_RW = SDL_LoadBMP_RW;
auto _fun_SDL_ConvertSurface = SDL_ConvertSurface;
auto _fun_SDL_CreateTextureFromSurface = SDL_CreateTextureFromSurface;
auto _fun_SDL_FreeSurface = SDL_FreeSurface;
auto _fun_SDL_PollEvent = SDL_PollEvent;
auto _fun_SDL_RenderClear = SDL_RenderClear;
auto _fun_SDL_RenderCopy = SDL_RenderCopy;
auto _fun_SDL_RenderPresent = SDL_RenderPresent;
int _val_SDL_QUIT = SDL_QUIT;
int _val_SDL_KEYDOWN = SDL_KEYDOWN;
int _val_SDLK_UP = SDLK_UP;
int _val_SDLK_DOWN = SDLK_DOWN;
int _val_SDLK_LEFT = SDLK_LEFT;
int _val_SDLK_RIGHT = SDLK_RIGHT;

/* int main(int argc, char *argv[]) { */
/*     SDL_Window *win = NULL; */
/*     SDL_Renderer *renderer = NULL; */
/*     SDL_Texture *bitmapTex = NULL; */
/*     SDL_Surface *bitmapSurface = NULL; */
/*     /\* SDL_Surface *tempSurface = NULL; *\/ */
/*     SDL_Surface *screenSurface = NULL; */
/*     int posX = 100, posY = 100, width = 640, height = 480; */

/*     SDL_Init(SDL_INIT_VIDEO); */

/*     win = SDL_CreateWindow("Hello World", posX, posY, width, height, 0); */

/*     screenSurface = SDL_GetWindowSurface(win); */
/*     if(!screenSurface) */
/*       { */
/*  	printf("%s\n", SDL_GetError()); */
/*  	goto cleanup3; */
/*       } */
/*     renderer = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED); */
/*     if(!renderer) */
/*       { */
/*     	printf("%s\n", SDL_GetError()); */
/*     	goto cleanup2; */
/*       } */
/*     SDL_SetRenderDrawColor( renderer, 0xFF, 0xFF, 0xFF, 0xFF ); */

/*     bitmapSurface = SDL_LoadBMP("ship.bmp"); */
/*     if(!bitmapSurface) */
/*       { */
/*  	printf("%s\n", SDL_GetError()); */
/*  	goto cleanup; */
/*       } */
/*     /\* bitmapSurface = SDL_ConvertSurface(tempSurface, screenSurface->format, 0); *\/ */
/*     /\* SDL_FreeSurface(tempSurface); *\/ */
/*     /\* if(!bitmapSurface) *\/ */
/*     /\*   { *\/ */
/*     /\* 	printf("%s\n", SDL_GetError()); *\/ */
/*     /\* 	goto cleanup; *\/ */
/*     /\*   } *\/ */
/*     bitmapTex = SDL_CreateTextureFromSurface(renderer, bitmapSurface); */
/*     SDL_Rect rect; */
/*     rect.x = 10; */
/*     rect.y = 20; */
/*     rect.w = bitmapSurface->w; */
/*     rect.h = bitmapSurface->h; */
/*     if(!bitmapTex) */
/*       { */
/*     	printf("%s\n", SDL_GetError()); */
/*     	goto cleanup; */
/*       } */
/*        SDL_FreeSurface(bitmapSurface); */

/*        int dx; */
/*        int dy; */

/*     while (1) { */
/*         SDL_Event e; */
/*         if (SDL_PollEvent(&e)) { */
/*             if (e.type == SDL_QUIT) { */
/*                 break; */
/*             } else { */
/*  	      if (e.type == SDL_KEYDOWN) */
/*  		{ */
/*                         switch( e.key.keysym.sym ) */
/*                         { */
/*                             case SDLK_UP: */
/*  			      dx = 0; */
/*  			      dy = -1; */
/*                             break; */

/*                             case SDLK_DOWN: */
/*  			      dx = 0; */
/*  			      dy = 1; */
/*                             break; */

/*                             case SDLK_LEFT: */
/*  			      dx = -1; */
/*  			      dy = 0; */
/*                             break; */

/*                             case SDLK_RIGHT: */
/*  			      dx = 1; */
/*  			      dy = 0; */
/*                             break; */

/*                             default: */
/*  			      dx = 0; */
/*  			      dy = 0; */
/*                             break; */
/*                         }		} */
/*  	    } */
/*         } */

/*  	/\* SDL_BlitSurface(bitmapSurface, NULL, screenSurface, NULL); *\/ */
/*  	/\* SDL_UpdateWindowSurface(win); *\/ */
/*  	rect.x += dx; */
/*  	rect.y += dy; */

/*  	rect.x = rect.x > (width - rect.w) ? (width - rect.w) : rect.x; */
/*  	rect.x = rect.x < 0 ? 0 : rect.x; */
/*  	rect.y = rect.y > (height - rect.h) ? (height - rect.h) : rect.y; */
/*  	rect.y = rect.y < 0 ? 0 : rect.y; */

/*         SDL_RenderClear(renderer); */
/*         SDL_RenderCopy(renderer, bitmapTex, NULL, &rect); */
/*         SDL_RenderPresent(renderer); */
/*     } */
/*  /\* cleanup0: *\/ */
/*     SDL_DestroyTexture(bitmapTex); */
/*  cleanup: */
/*     SDL_DestroyRenderer(renderer); */
/*  cleanup2: */
/*     SDL_DestroyWindow(win); */
/*  cleanup3: */
/*     SDL_Quit(); */

/*     return 0; */
/* } */
