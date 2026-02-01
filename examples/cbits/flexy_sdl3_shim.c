#include <SDL3/SDL.h>
#include <stdint.h>

uint32_t flexy_sdl_init_video_flag(void)
{
    return SDL_INIT_VIDEO;
}

int flexy_sdl_poll_event(int *out_code)
{
    SDL_Event e;
    if (!SDL_PollEvent(&e)) {
        return 0;
    }

    if (e.type == SDL_EVENT_QUIT) {
        *out_code = 1; /* quit */
        return 1;
    }

    if (e.type == SDL_EVENT_KEY_DOWN) {
        if (e.key.key == SDLK_ESCAPE) {
            *out_code = 1; /* quit */
            return 1;
        }
        if (e.key.key == SDLK_D) {
            *out_code = 2; /* toggle */
            return 1;
        }
    }

    *out_code = 0;
    return 1;
}
