import tkinter as tk
from tkinter import ttk, messagebox

PARTICULAS_DB = {
    "Bala (id: 2000)":      {"dmg": 20.0},
    "Fuego (id: 2003)":     {"dmg": 20.0},
    "Plasma (id: 2004)":    {"dmg": 45.0},
    "Cohete (id: 2005)":    {"dmg": 250.0},
    "Chispa (id: 2001)":    {"dmg": 50.0},
    "Humo (id: 2002)":      {"dmg": 0.0},
}

ARMAS_DEFAULT = [
    ("Glock 17",        200.0,  1.43),
    ("Escopeta 12G",    120.0,  20.0),
    ("Fusil M52",       400.0,  1.18),
    ("Lanzallamas MX",  5000.0, 0.74),
    ("Sniper M24",      13.3,   6.67),
    ("Subfusil Uzi",    333.3,  999.0),
    ("RPG-7",           125.0,  2.86),
    ("Plasma XV",       300.0,  15.0),
    ("Minigun M134",    500.0,  999.0)
]

BUFFS_DEFAULT = [
    ("Poción Vida (A)", 15.0, 375.0),
    ("Poción Mayor (B)", 6.0,  300.0),
    ("Elixir Comp (C)", 1.3,  133.3),
    ("Reloj Arena (A)", 24.0, 240.0),
    ("Reloj Mist (B)",  4.8,  96.0),
    ("Crono-Cristal(C)",1.5,  45.0)
]

class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Herramienta de Calibración Inversa")
        self.geometry("1000x600")

        style = ttk.Style()
        style.theme_use('clam')

        self.notebook = ttk.Notebook(self)
        self.notebook.pack(expand=True, fill='both', padx=10, pady=10)

        self.tab_armas = tk.Frame(self.notebook)
        self.tab_buffs = tk.Frame(self.notebook)
        
        self.notebook.add(self.tab_armas, text="Calibración Armas")
        self.notebook.add(self.tab_buffs, text="Calibración Buffs")

        self.setup_armas_tab()
        self.setup_buffs_tab()

    def setup_armas_tab(self):
        left_panel = tk.Frame(self.tab_armas, width=250, bg="#f0f0f0", relief="sunken", bd=1)
        left_panel.pack(side="left", fill="y", padx=5, pady=5)
        left_panel.pack_propagate(False)

        tk.Label(left_panel, text="Partícula Base (Variable Fija)", bg="#f0f0f0", font=("Arial", 10, "bold")).pack(pady=(20, 5))
        self.combo_particulas = ttk.Combobox(left_panel, values=list(PARTICULAS_DB.keys()), state="readonly")
        self.combo_particulas.current(0)
        self.combo_particulas.pack(padx=10, fill="x")

        tk.Label(left_panel, text="CoolRate Base (Estimado)", bg="#f0f0f0", font=("Arial", 9)).pack(pady=(15, 0))
        self.entry_coolrate_base = ttk.Entry(left_panel)
        self.entry_coolrate_base.insert(0, "30.0") 
        self.entry_coolrate_base.pack(padx=10, pady=5)
        tk.Label(left_panel, text="(Usado para despejar HeatPerShot)", bg="#f0f0f0", font=("Arial", 7)).pack()

        tk.Label(left_panel, text="STATS CALCULADOS", bg="#f0f0f0", font=("Arial", 12, "bold"), fg="blue").pack(pady=(40, 10))
        self.lbl_stats_arma = tk.Label(left_panel, text="Selecciona un arma...", bg="#e0e0e0", justify="left", relief="groove", bd=2, padx=10, pady=10)
        self.lbl_stats_arma.pack(padx=10, fill="both", expand=True, pady=(0, 20))

        right_panel = tk.Frame(self.tab_armas)
        right_panel.pack(side="right", fill="both", expand=True, padx=5, pady=5)

        headers = ["Arma (Calcular)", "DPS Objetivo", "Tiempo Overheat (s)", "Daño en ese tiempo"]
        for col, text in enumerate(headers):
            lbl = tk.Label(right_panel, text=text, font=("Arial", 9, "bold"))
            lbl.grid(row=0, column=col, padx=5, pady=5)

        self.arma_entries = []
        for i, (nombre, dps, tiempo) in enumerate(ARMAS_DEFAULT):
            row = i + 1

            btn = tk.Button(right_panel, text=nombre, width=20, command=lambda idx=i: self.calcular_arma(idx))
            btn.grid(row=row, column=0, padx=5, pady=2)

            ent_dps = ttk.Entry(right_panel, width=15)
            ent_dps.insert(0, str(dps))
            ent_dps.grid(row=row, column=1, padx=5)

            ent_time = ttk.Entry(right_panel, width=15)
            ent_time.insert(0, str(tiempo))
            ent_time.grid(row=row, column=2, padx=5)

            daño_total = dps * tiempo
            ent_dmg = ttk.Entry(right_panel, width=15)
            if tiempo >= 999:
                 ent_dmg.insert(0, "Infinito")
            else:
                 ent_dmg.insert(0, str(int(daño_total)))
            ent_dmg.grid(row=row, column=3, padx=5)

            self.arma_entries.append({
                "name": nombre,
                "dps": ent_dps,
                "time": ent_time,
                "dmg": ent_dmg
            })

    def calcular_arma(self, idx):
        try:
            entries = self.arma_entries[idx]
            nombre = entries["name"]

            dps_target = float(entries["dps"].get())
            time_target = float(entries["time"].get())
            
            nom_part = self.combo_particulas.get()
            dmg_part = PARTICULAS_DB[nom_part]["dmg"]

            cool_rate_base = float(self.entry_coolrate_base.get())
            if dps_target <= 0: raise ValueError("DPS debe ser mayor a 0")
            fire_rate = dmg_part / dps_target

            
            if time_target >= 999:
                heat_per_shot = (cool_rate_base * fire_rate) * 0.95
                net_heat = heat_per_shot - (cool_rate_base * fire_rate)
                status_msg = "Sostenido (Infinito)"
            else:
                net_heat = (100.0 * fire_rate) / time_target
                heat_per_shot = net_heat + (cool_rate_base * fire_rate)
                status_msg = f"{time_target} seg"

            res_text = (
                f"Arma: {nombre}\n"
                f"----------------------------\n"
                f"Partícula: {nom_part}\n"
                f"Daño Base: {dmg_part}\n\n"
                f"--- STATS SUGERIDOS ---\n"
                f"FireRate (Delay): {fire_rate:.4f} s\n"
                f"Heat Per Shot:    {heat_per_shot:.2f}\n"
                f"Cool Rate:        {cool_rate_base:.2f}\n"
                f"Max Heat:         100.0\n\n"
                f"--- VERIFICACIÓN ---\n"
                f"Tiempo real calc: {status_msg}"
            )
            
            self.lbl_stats_arma.config(text=res_text, fg="black")
            if time_target < 999:
                new_dmg = dps_target * time_target
                entries["dmg"].delete(0, tk.END)
                entries["dmg"].insert(0, str(int(new_dmg)))

        except ValueError as e:
            messagebox.showerror("Error de Cálculo", f"Revisa los valores numéricos.\nError: {e}")

    def setup_buffs_tab(self):
        left_panel = tk.Frame(self.tab_buffs, width=250, bg="#f0f0f0", relief="sunken", bd=1)
        left_panel.pack(side="left", fill="y", padx=5, pady=5)
        left_panel.pack_propagate(False)

        tk.Label(left_panel, text="STATS SPAWNER", bg="#f0f0f0", font=("Arial", 12, "bold"), fg="green").pack(pady=(40, 10))
        self.lbl_stats_buff = tk.Label(left_panel, text="Selecciona un buff...", bg="#e0e0e0", justify="left", relief="groove", bd=2, padx=10, pady=10)
        self.lbl_stats_buff.pack(padx=10, fill="both", expand=True, pady=(0, 20))

        right_panel = tk.Frame(self.tab_buffs)
        right_panel.pack(side="right", fill="both", expand=True, padx=5, pady=5)

        headers = ["Buff / Item", "Apariciones / min", "Valor Total / min"]
        for col, text in enumerate(headers):
            lbl = tk.Label(right_panel, text=text, font=("Arial", 9, "bold"))
            lbl.grid(row=0, column=col, padx=5, pady=5)

        self.buff_entries = []
        for i, (nombre, rate, val_total) in enumerate(BUFFS_DEFAULT):
            row = i + 1
            
            btn = tk.Button(right_panel, text=nombre, width=20, command=lambda idx=i: self.calcular_buff(idx))
            btn.grid(row=row, column=0, padx=5, pady=2)

            ent_rate = ttk.Entry(right_panel, width=15)
            ent_rate.insert(0, str(rate))
            ent_rate.grid(row=row, column=1, padx=5)

            ent_val = ttk.Entry(right_panel, width=15)
            ent_val.insert(0, str(val_total))
            ent_val.grid(row=row, column=2, padx=5)

            self.buff_entries.append({
                "name": nombre,
                "rate": ent_rate,
                "val": ent_val
            })

    def calcular_buff(self, idx):
        try:
            entries = self.buff_entries[idx]
            nombre = entries["name"]
            
            rate_min = float(entries["rate"].get())
            total_val_min = float(entries["val"].get())

            if rate_min <= 0: raise ValueError("Apariciones/min > 0")
            valor_unitario = total_val_min / rate_min
            tiempo_promedio = 60.0 / rate_min
            t_min = 0.0
            t_max = 2 * tiempo_promedio

            res_text = (
                f"Buff: {nombre}\n"
                f"----------------------------\n"
                f"Meta: {rate_min} spawns/min\n"
                f"Meta: {total_val_min} valor/min\n\n"
                f"--- STATS SUGERIDOS ---\n"
                f"Valor Item:       {int(valor_unitario)}\n"
                f"Spawner T. Min:   {t_min:.1f} s\n"
                f"Spawner T. Max:   {t_max:.1f} s\n"
                f"(Promedio: {tiempo_promedio:.1f} s)"
            )
            
            self.lbl_stats_buff.config(text=res_text, fg="black")

        except ValueError as e:
            messagebox.showerror("Error", f"Verifica los valores.\n{e}")

if __name__ == "__main__":
    app = App()
    app.mainloop()