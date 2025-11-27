import tkinter as tk
from tkinter import simpledialog, messagebox, scrolledtext

import mapmaker as mp
import mapmaker.haskell as mph

class MainApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Haskell Map Generator - Modular")
        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=0)
        self.rowconfigure(1, weight=1)              
        
        self.map_canvas = mp.MapCanvas(self, width=800, height=600)
        self.controls = mp.ControlPanel(self)
        self.controls.btn_export.configure(command=self.mostrar_resultado)
        self.controls.btn_clear.configure(command=self.map_canvas.clear_all)
        self.controls.btn_import.configure(command=self.importar_codigo)

        self.controls.grid(row=0, column=0, sticky=tk.EW, padx=10, pady=(10,0))
        self.map_canvas.grid(row=1, column=0, sticky=tk.NSEW, padx=10, pady=(0,10))

    def mostrar_resultado(self):
        top = tk.Toplevel(self)
        top.title("Código Haskell Generado")
        txt = scrolledtext.ScrolledText(top, width=80, height=20)

        texto = self.map_canvas.obstacles
        texto = mph.generar_codigo_haskell(texto)

        txt.pack(padx=10, pady=10)
        txt.insert(tk.END, texto)
        txt.config(state=tk.DISABLED)

    def importar_codigo(self):
        top = tk.Toplevel(self)
        top.title("Importar Código Haskell")
        txt = scrolledtext.ScrolledText(top, width=80, height=20)
        btn_frame = tk.Frame(top)

        txt.pack(padx=10, pady=10)
        btn_frame.pack(fill=tk.X, padx=10, pady=10)
        
        def procesar_importacion():
            codigo_raw = txt.get("1.0", tk.END)
            datos = mph.parsear_codigo_haskell(codigo_raw)
            
            if not datos: return messagebox.showwarning("Importar", "No se encontraron obstáculos válidos en el texto.")
            self.map_canvas.clear_all()
            for (x, y, w, h) in datos: self.map_canvas.add_obstacle(x, y, w, h)
                
            messagebox.showinfo("Éxito", f"Se han importado {len(datos)} obstáculos.")
            top.destroy()
            
        btn_ok = tk.Button(btn_frame, text="Renderizar en Canvas", command=procesar_importacion)
        btn_ok.pack(side=tk.RIGHT)

if __name__ == "__main__":
    app = MainApp()
    app.mainloop()