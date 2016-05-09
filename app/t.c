// clang t.c -lcygwin -lSDL2main -lSDL2
// clang -std=c++11 -S -emit-llvm

#include <stdio.h>
#include <stdbool.h>
#include <SDL2/SDL.h>

#define WIN_TITLE "hello, world"
#define WIN_X 2000
#define WIN_Y 50
#define WIN_WIDTH 1800
#define WIN_HEIGHT 980

#define X_ACC 211380.0f
#define Y_ACC X_ACC

#define MAX_OBJS 10000
#define MAX_TIMERS 1000

#define NUM_UFOS 3000

/* would like to tie the TID and string together */
#define SHIP_TID 0
#define UFO_TID 1
#define PHOTON_TID 2

/* run-time constant array, init in main */
SDL_Texture *g_tex[] = { (SDL_Texture *)"ship.bmp", (SDL_Texture *)"ufo.bmp", (SDL_Texture *)"photon.bmp" };

#define NUM_TEX (sizeof(g_tex)/sizeof(SDL_Texture*))
int g_tex_w[NUM_TEX];
int g_tex_h[NUM_TEX];

SDL_Window *g_window = NULL;
SDL_Renderer *g_renderer = NULL;
SDL_Surface *g_screen = NULL;

/* push to the end, delete from anywhere (z ordered?) */
int g_nobjs = 0;
int g_obj_tid[MAX_OBJS];
float g_obj_x[MAX_OBJS];
float g_obj_y[MAX_OBJS];
float g_obj_vx[MAX_OBJS];
float g_obj_vy[MAX_OBJS];
float g_obj_ax[MAX_OBJS];
float g_obj_ay[MAX_OBJS];

/* fifo ring buffer */
int g_ntimers = 0;
Uint64 g_timer_expire[MAX_OBJS];
int g_timer_oid[MAX_OBJS];

#define OBJ_X(i) g_obj_x[i]
#define OBJ_Y(i) g_obj_y[i]
#define OBJ_W(i) g_tex_w[g_obj_tid[i]]
#define OBJ_H(i) g_tex_h[g_obj_tid[i]]
#define OBJ_TEX(i) g_tex[g_obj_tid[i]]

#define OBJ_X1(i) OBJ_X(i) + OBJ_W(i)
#define OBJ_Y1(i) OBJ_Y(i) + OBJ_H(i)

Uint64 g_perf_freq; /* architecture dependent constant */

void die_if(bool r, char *s);

int colliding(int i, int j)
{
  return (((OBJ_X(i) <= OBJ_X1(j)) && (OBJ_X1(i) >= OBJ_X(j))) &&
	  ((OBJ_Y(i) <= OBJ_Y1(j)) && (OBJ_Y1(i) >= OBJ_Y(j))));
}

int alloc_obj(int tid)
{
  int i = g_nobjs;
  
  die_if(i >= MAX_OBJS, "unable to allocate object");
  
  ++g_nobjs;
  
  g_obj_tid[i] = tid;
  g_obj_x[i] = WIN_WIDTH / 2.0f;
  g_obj_y[i] = WIN_HEIGHT / 2.0f;
  g_obj_vx[i] = 0.0f;
  g_obj_vy[i] = 0;
  g_obj_ax[i] = 0.0f;
  g_obj_ay[i] = 0;
  
  return i;
}

void alloc_timer_1s(int oid, Uint64 t)
{
  int i = g_ntimers;
  printf("alloc_timer %d\n", i);
  die_if(i >= MAX_TIMERS, "unable to allocate timer");
  ++g_ntimers;
  g_timer_expire[i] = t + g_perf_freq;
  g_timer_oid[i] = oid;
}

void dealloc_obj(int i)
{
  //  printf("dealloc_obj %d\n", i);

  int n = g_nobjs;
  
  die_if(i >= n, "unknown object index");

  --n;

  g_nobjs = n;
  
  if(n == 0 || i == n) { return; }

  g_obj_tid[i] = g_obj_tid[n];

  g_obj_x[i] = g_obj_x[n];
  g_obj_y[i] = g_obj_y[n];

  g_obj_vx[i] = g_obj_vx[n];
  g_obj_vy[i] = g_obj_vy[n];

  g_obj_ax[i] = g_obj_ax[n];
  g_obj_ay[i] = g_obj_vy[n];

}

void dealloc_timer_1s(int i)
{
  printf("dealloc_timer %d\n", i);
  die_if(i >= g_ntimers, "unknown timer index");

  --g_ntimers;

  int n = g_ntimers;

  dealloc_obj(g_timer_oid[i]);

  if(n == 0 || i == n) { return; }

  g_timer_oid[i] = g_timer_oid[n];
  g_timer_expire[i] = g_timer_expire[n];
  
}

void update(float *px, float *pv, float *pa, float t, float min, int max)
{
float x = *px;
float v = *pv;
float a = *pa;

x = x + v*t + a*0.5*t*t; /* redundant calculation */
 *px = x < min ? (x + max) : (x > max) ? (x - max) : x;
*pv = v + a*t;
*pa = 0.0f;
}

void exit_cleanup(int e)
{

  for(int i = 0; i < g_nobjs; ++i) { dealloc_obj(i); }

  for(unsigned int i = 0; i < NUM_TEX; ++i) { SDL_DestroyTexture(g_tex[i]); }

  if(g_renderer) { SDL_DestroyRenderer(g_renderer); }
  if(g_screen) { SDL_FreeSurface(g_screen); }
  if(g_window) { SDL_DestroyWindow(g_window); }

  SDL_Quit();
  exit(e);
}

void die_if(bool r, char *s)
{
  if(r)
    {
      printf("%s\n", s);
      printf("%s\n", SDL_GetError());
      exit_cleanup(1);
    }
}

void render(int i, SDL_Texture *tex) /* inline? */
{
  SDL_Rect rect;
  rect.x = g_obj_x[i];
  rect.y = g_obj_y[i];
  rect.w = OBJ_W(i);
  rect.h = OBJ_H(i);

  //	SDL_RenderCopy(g_renderer, tex, NULL, &rect) /* ignore return */;
  SDL_RenderCopyEx(g_renderer, tex, NULL, &rect, 0.0, NULL, SDL_FLIP_NONE); /* ignore return */;
}

int main() {
  
  die_if(SDL_Init(SDL_INIT_VIDEO), "unable to init SDL");
  die_if(!(g_window = SDL_CreateWindow(WIN_TITLE, WIN_X, WIN_Y, WIN_WIDTH, WIN_HEIGHT, 0)),
	 "unable to create window");
  die_if(!(g_screen = SDL_GetWindowSurface(g_window)), "unable to create window surface");
  die_if(!(g_renderer = SDL_CreateRenderer(g_window, -1, SDL_RENDERER_ACCELERATED)),
	 "unable to create renderer");

  die_if(SDL_SetRenderDrawColor( g_renderer, 0x0, 0x0, 0x0, 0xFF ),
	 "unable to set draw color");

  g_perf_freq = SDL_GetPerformanceFrequency();

  // init textures
{
  SDL_Surface *srfc = NULL;
  for(unsigned int i = 0; i < NUM_TEX; ++i)
    {
      die_if(!(srfc = SDL_LoadBMP((char*)g_tex[i])), "unable to load bitmap");
      die_if(!(g_tex[i] = SDL_CreateTextureFromSurface(g_renderer, srfc))
	     , "unable to create bmp texture");

      g_tex_w[i] = srfc->w;
      g_tex_h[i] = srfc->h;
  
      SDL_FreeSurface(srfc);
    }
}
  
  alloc_obj(SHIP_TID);

int nufos = NUM_UFOS;

for(int i = 0; i < nufos; ++i)
{
  alloc_obj(UFO_TID);
}

  Uint64 t0 = SDL_GetPerformanceCounter();

/* used to optimize (minimize) calls to rand() */
#define HASBITS(x,y) (((x) & (y)) == (y))

/* int foo = 0; */
/* for(int i = 0; i < 100000; ++i) */
/* { */
/* int r = rand(); */
/* if (HASBITS(r,0x7) || HASBITS(r,0x70)) ++foo; */
/* } */

// printf("%d\n", foo);
// exit(-1);
while (1) {
// SDL_Delay(10);
    SDL_Event e;

    Uint64 t1 = SDL_GetPerformanceCounter();
    float dt;
    dt = (float)(t1 - t0)/g_perf_freq;
    t0 = t1;

    /* user events */
    if (SDL_PollEvent(&e)) {
      if (e.type == SDL_QUIT) { exit_cleanup(0); }
      if (e.type == SDL_KEYDOWN)
	{
          SDL_Keycode sym = e.key.keysym.sym;
	  if(sym == SDLK_SPACE)
	    {
	      alloc_timer_1s(alloc_obj(PHOTON_TID), t1);
	    }
	  g_obj_ax[0] = sym == SDLK_LEFT ? -(X_ACC) : (sym == SDLK_RIGHT ? X_ACC : 0.0f);
	  g_obj_ay[0] = sym == SDLK_UP ? -(Y_ACC) : (sym == SDLK_DOWN ? Y_ACC : 0.0f);
	}
    }

    /* ai events */
    for(int i = 1; i < nufos + 1; ++i)
      {
	int r = rand();
#define RAND_FREQ 0xf
	g_obj_ax[i] = HASBITS(r,RAND_FREQ) ? X_ACC : 0.0f;
	g_obj_ax[i] += HASBITS(r,RAND_FREQ << 8) ? -(X_ACC) : 0.0f;
	g_obj_ax[i] /= 100.0f;
	g_obj_ay[i] = HASBITS(r,RAND_FREQ << 16) ? Y_ACC : 0.0f;
	g_obj_ay[i] += HASBITS(r,RAND_FREQ << 24) ? -(Y_ACC) : 0.0f;
	g_obj_ay[i] /= 100.0f;
      }


    /* timer events */
    for(int i = 0; i < g_ntimers; ++i)
      {
	if(t1 >= g_timer_expire[i])
	  {
	    dealloc_timer_1s(i);
	  }
      }

    /* physics update */
    
    for(int i = g_nobjs - 1; i >= 0 ; --i)
      {
        update(&g_obj_x[i], &g_obj_vx[i], &g_obj_ax[i], dt, -OBJ_W(i), WIN_WIDTH);
        update(&g_obj_y[i], &g_obj_vy[i], &g_obj_ay[i], dt, -OBJ_H(i), WIN_HEIGHT);
      }

    /* collisions */
    for(int i = g_nobjs - 1; i >= 0 ; --i)
      {
	for(int j = i - 1; j >= 0 ; --j)
	  {
	    if (colliding(i,j))
	      {
	      }
	  }
      }
    /* rendering */
    SDL_RenderClear(g_renderer); /* ignore return */

    SDL_Texture *tex = g_tex[UFO_TID];
      
    for(int i = 1; i < nufos + 1; ++i)
      {
	render(i, tex);
      }
    render(0, g_tex[SHIP_TID]);
    tex = g_tex[PHOTON_TID];
    for(int i = 1 + nufos; i < 1 + nufos + g_ntimers; ++i)
      {
	render(i, tex);
      }

    SDL_RenderPresent(g_renderer);
  }
 
  return 0;
}
