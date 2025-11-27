import re
from tkinter import simpledialog, messagebox

def generar_codigo_haskell(obstacles:list[dict]) -> str:
    """Recibe una lista de dicts y retorna código Haskell."""
    if not obstacles: return "mapaBox :: [Types.Obstaculo]\nmapaBox = []"

    code_header = "mapaBox :: [Types.Obstaculo]\nmapaBox = \n    [ "
    lines = []
    required_keys = ('x', 'y', 'w', 'h')

    for i, obs in enumerate(obstacles):
        if not all(key in obs for key in required_keys):
            print(f"[Warn] Obstáculo #{i} ignorado: Faltan claves. Datos: {obs}")
            continue

        try:
            x = float(obs['x'])
            y = float(obs['y'])
            w = float(obs['w'])
            h = float(obs['h'])
            line = f"Types.Obstaculo (SDL.V2 {x:.1f} {y:.1f}) (SDL.V2 {w:.1f} {h:.1f})"
            lines.append(line)
            
        except (ValueError, TypeError):
            print(f"[Warn] Obstáculo #{i} ignorado: Valores no numéricos. Datos: {obs}")
            continue

    if not lines: return "mapaBox :: [Types.Obstaculo]\nmapaBox = [] -- No se encontraron datos válidos"
    else: return code_header + "\n    , ".join(lines) + "\n    ]"

def parsear_codigo_haskell(codigo: str) -> list[tuple[float, float, float, float]]:
    patron = r"Types\.Obstaculo\s*\(SDL\.V2\s+([-\d\.]+)\s+([-\d\.]+)\)\s*\(SDL\.V2\s+([-\d\.]+)\s+([-\d\.]+)\)"
    
    coincidencias = re.findall(patron, codigo)
    resultados = []
    
    for match in coincidencias:
        try:
            # Convertimos los grupos capturados a float
            x, y = float(match[0]), float(match[1])
            w, h = float(match[2]), float(match[3])
            resultados.append((x, y, w, h))
        except ValueError:
            continue
            
    return resultados

def create_obstacle_dialog(cx:int, cy:int) -> tuple[int, int, int, int]:
    """Levanta una ventana de tkinter preguntando por ancho y alto."""
    dims = simpledialog.askstring("Nuevo Obstáculo", "Ingresa 'Ancho Alto' (ej: 100 50):")
    if dims:
        try:
            w_str, h_str = dims.split()
            w, h = float(w_str), float(h_str)
            return (cx - (w / 2), cy - (h / 2), w, h)
        
        except ValueError:
            messagebox.showerror("Error", "Formato inválido.")
            return (cx, cy, 10, 10)