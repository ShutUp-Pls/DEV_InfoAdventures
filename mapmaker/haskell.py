import re
from tkinter import simpledialog, messagebox

def generar_codigo_haskell(obstacles:list[dict]) -> str:
    if not obstacles: return "mapaBox :: [GType.Box]\nmapaBox = []"

    # Preguntar al usuario qué formato prefiere para la salida
    exportar_como_top_left = messagebox.askyesno(
        "Formato de Exportación",
        "¿Deseas exportar las coordenadas como ESQUINA SUPERIOR IZQUIERDA (Top-Left)?\n\n"
        "SÍ: Compatible con el motor SDL actual (Recomendado).\n"
        "NO: Exportar coordenadas de CENTRO (Nuevo formato)."
    )

    code_header = "mapaBox :: [GType.Box]\nmapaBox = \n    [ "
    lines = []
    required_keys = ('x', 'y', 'w', 'h')

    # Helper para formatear numeros
    def fmt(val):
        return f"({val:.1f})" if val < 0 else f"{val:.1f}"

    for i, obs in enumerate(obstacles):
        if not all(key in obs for key in required_keys): continue
        try:
            # En Python (Canvas) siempre trabajamos con CENTRO (cx, cy)
            w, h = float(obs['w']), float(obs['h'])
            cx, cy = float(obs['x']), float(obs['y'])
            angle = float(obs.get('angle', 0.0))
            
            if exportar_como_top_left:
                # Convertir Centro -> Top-Left para el archivo
                final_x = cx - (w / 2)
                final_y = cy - (h / 2)
            else:
                # Escribir Centro directamente
                final_x = cx
                final_y = cy

            line = f"GType.Box (SDL.V2 {fmt(final_x)} {fmt(final_y)}) (SDL.V2 {fmt(w)} {fmt(h)}) {fmt(angle)} 0"
            lines.append(line)
        except (ValueError, TypeError): continue

    if not lines: return "mapaBox :: [GType.Box]\nmapaBox = []"
    else: return code_header + "\n    , ".join(lines) + "\n    ]"

def parsear_codigo_haskell(codigo: str) -> list[tuple[float, float, float, float, float]]:
    # Preguntar al usuario el formato del archivo de entrada
    es_top_left = messagebox.askyesno(
        "Formato de Importación",
        "¿El código a importar usa coordenadas de ESQUINA (Top-Left)?\n\n"
        "SÍ: Convertir a Centro (El mapa se ajustará para el editor).\n"
        "NO: Leer como Centro (Sin cambios)."
    )

    # Regex para capturar floats que pueden tener parentesis: (?: \( (patron) \) | (patron) )
    num_pat = r"\(?([-\d\.]+)\)?" 
    patron = rf"GType\.Box\s*\(SDL\.V2\s+{num_pat}\s+{num_pat}\)\s*\(SDL\.V2\s+{num_pat}\s+{num_pat}\)\s*{num_pat}\s+0"
    
    coincidencias = re.findall(patron, codigo)
    resultados = []
    
    for match in coincidencias:
        try:
            # Leemos las coordenadas crudas del archivo
            raw_x, raw_y = float(match[0]), float(match[1])
            w, h = float(match[2]), float(match[3])
            angle = float(match[4])
            
            if es_top_left:
                # Si el archivo es Top-Left, calculamos el Centro para el Canvas
                cx = raw_x + (w / 2)
                cy = raw_y + (h / 2)
            else:
                # Si el archivo ya es Centro, lo usamos directo
                cx = raw_x
                cy = raw_y
            
            resultados.append((cx, cy, w, h, angle))
        except ValueError: continue
    return resultados

def create_obstacle_dialog(cx:int, cy:int) -> tuple[float, float, float, float]:
    dims = simpledialog.askstring("Nuevo Obstáculo", "Ingresa 'Ancho Alto' (ej: 100 50):")
    if dims:
        try:
            w, h = map(float, dims.split())
            # Retornamos Top-Left relativo al click para mantener compatibilidad con la logica de creacion
            return (cx - w/2, cy - h/2, w, h)
        except ValueError:
            messagebox.showerror("Error", "Formato inválido.")
            return (cx - 5, cy - 5, 10, 10)
    return None