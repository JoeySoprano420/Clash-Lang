import pygame

def parse_behavior_script(script="input.clsh"):
    with open(script) as f:
        lines = [line.strip() for line in f if "print" in line]
    actions = []
    for line in lines:
        if "print(" in line:
            msg = line.split("print(")[1].split(")")[0].strip('"')
            actions.append(msg)
    return actions

def run_game():
    pygame.init()
    screen = pygame.display.set_mode((640, 480))
    pygame.display.set_caption("Clashup Game Runtime")
    font = pygame.font.SysFont("Courier", 20)

    actions = parse_behavior_script()
    action_index = 0
    clock = pygame.time.Clock()

    running = True
    while running:
        screen.fill((0, 0, 0))
        for e in pygame.event.get():
            if e.type == pygame.QUIT:
                running = False
        if action_index < len(actions):
            msg = actions[action_index]
            text = font.render(f"💬 {msg}", True, (255, 255, 0))
            screen.blit(text, (50, 220))
            action_index += 1
        pygame.display.flip()
        clock.tick(1)

if __name__ == "__main__":
    run_game()
