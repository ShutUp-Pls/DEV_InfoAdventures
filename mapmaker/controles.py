import tkinter as tk
from tkinter import scrolledtext

class ControlPanel(tk.Frame):
    def __init__(self, parent):
        super().__init__(parent)

        self.center_frame = tk.Frame(self)
        self.center_frame.pack()

        self.btn_export = tk.Button(self.center_frame, text="Generar Código Haskell", bg="#4CAF50", fg="white")
        self.btn_export.grid(row=0, column=0, sticky=tk.NSEW)

        self.btn_import = tk.Button(self.center_frame, text="Importar Código Haskell", bg="#CFCC25", fg="white")
        self.btn_import.grid(row=0, column=1, sticky=tk.NSEW)
        
        self.btn_clear = tk.Button(self.center_frame, text="Limpiar Todo", bg="#f44336", fg="white")
        self.btn_clear.grid(row=0, column=2, sticky=tk.NSEW)