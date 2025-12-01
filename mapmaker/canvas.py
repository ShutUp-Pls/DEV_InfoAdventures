import tkinter as tk
from tkinter import simpledialog
import math
from .haskell import create_obstacle_dialog

class MapCanvas(tk.Frame):
    def __init__(self, parent, width=800, height=600):
        super().__init__(parent)
        self.WIDTH = width
        self.HEIGHT = height
        self.current_scale = 1.0
        self.obstacles = []
        self.selected_item = None
        self.current_rect = None
        self.drag_mode = None
        self.drag_start_x = 0
        self.drag_start_y = 0
        self.click_real_x = 0
        self.click_real_y = 0
        self.last_mouse_x = 0
        self.last_mouse_y = 0
        self.offset_x = 0
        self.offset_y = 0
        self.keys = {"w": False, "a": False, "s": False, "d": False, "shift_l": False, "shift_r": False}
        self.camera_speed = 5
        self.sprint_speed = 15
        self.acc_x = 0.0
        self.acc_y = 0.0
        
        self.canvas = tk.Canvas(self, width=self.WIDTH, height=self.HEIGHT, bg="white", cursor="cross")
        self.canvas.pack(padx=10, pady=10, expand=True, fill=tk.BOTH)
        self.canvas.config(scrollregion=(-5000, -5000, 5000, 5000), xscrollincrement=1, yscrollincrement=1)
        
        self.mouse_coords_text = self.canvas.create_text(10, 10, text="X: 0, Y: 0  |  Zoom: 100%", anchor="nw", fill="red", font=("Arial", 10, "bold"))
        
        events = [
            ("<Button-1>", self.on_click), ("<B1-Motion>", self.on_drag), ("<ButtonRelease-1>", self.on_release),
            ("<Motion>", self.on_mouse_move), ("<KeyPress>", self.on_key_press), ("<KeyRelease>", self.on_key_release),
            ("<minus>", self.zoom_out), ("<KP_Subtract>", self.zoom_out), ("<plus>", self.zoom_in), ("<KP_Add>", self.zoom_in),
            ("<period>", lambda e: self.rotate_selection(5)), ("<comma>", lambda e: self.rotate_selection(-5))
        ]
        for event, func in events: self.canvas.bind(event, func)
        self.canvas.focus_set()
        self.update_camera_loop()

    def to_world(self, screen_val): return screen_val / self.current_scale
    def to_screen(self, world_val): return world_val * self.current_scale

    def get_poly_coords(self, cx, cy, w, h, angle_deg):
        vis_cx, vis_cy = self.to_screen(cx), self.to_screen(cy)
        vis_w, vis_h = self.to_screen(w), self.to_screen(h)
        angle_rad = math.radians(angle_deg)
        cos_a, sin_a = math.cos(angle_rad), math.sin(angle_rad)
        dx_vals = [-vis_w/2, vis_w/2, vis_w/2, -vis_w/2]
        dy_vals = [-vis_h/2, -vis_h/2, vis_h/2, vis_h/2]
        points = []
        for dx, dy in zip(dx_vals, dy_vals):
            points.extend([vis_cx + dx * cos_a - dy * sin_a, vis_cy + dx * sin_a + dy * cos_a])
        return points

    def zoom_in(self, event): self.apply_zoom(1.1)
    def zoom_out(self, event): self.apply_zoom(0.9)

    def apply_zoom(self, factor):
        off_x, off_y = self.canvas.canvasx(self.last_mouse_x), self.canvas.canvasy(self.last_mouse_y)
        self.canvas.scale("obstacle", 0, 0, factor, factor)
        self.current_scale *= factor
        new_off_x, new_off_y = off_x * factor, off_y * factor
        self.canvas.xview_scroll(int(new_off_x - off_x), "units")
        self.canvas.yview_scroll(int(new_off_y - off_y), "units")
        self.refresh_all_visuals()
        self.update_hud(self.last_mouse_x, self.last_mouse_y)

    def refresh_all_visuals(self):
        for obs in self.obstacles:
            self.canvas.coords(obs['id'], *self.get_poly_coords(obs['x'], obs['y'], obs['w'], obs['h'], obs['angle']))
            self.canvas.coords(obs['text_id'], self.to_screen(obs['x']), self.to_screen(obs['y']))

    def on_key_press(self, event):
        if event.keysym.lower() in self.keys: self.keys[event.keysym.lower()] = True

    def on_key_release(self, event):
        if event.keysym.lower() in self.keys: self.keys[event.keysym.lower()] = False

    def update_camera_loop(self):
        current_speed = self.sprint_speed if (self.keys["shift_l"] or self.keys["shift_r"]) else self.camera_speed
        target_dx = -current_speed if self.keys["a"] else (current_speed if self.keys["d"] else 0)
        target_dy = -current_speed if self.keys["w"] else (current_speed if self.keys["s"] else 0)
        
        self.acc_x += target_dx
        self.acc_y += target_dy
        scroll_x, scroll_y = int(self.acc_x), int(self.acc_y)
        
        if scroll_x != 0 or scroll_y != 0:
            self.canvas.xview_scroll(scroll_x, "units")
            self.canvas.yview_scroll(scroll_y, "units")
            self.acc_x -= scroll_x
            self.acc_y -= scroll_y
            self.update_hud(self.last_mouse_x, self.last_mouse_y)
        self.after(16, self.update_camera_loop)

    def on_mouse_move(self, event):
        self.last_mouse_x, self.last_mouse_y = event.x, event.y
        self.update_hud(event.x, event.y)

    def update_hud(self, screen_x, screen_y):
        real_x = int(self.to_world(self.canvas.canvasx(screen_x)))
        real_y = int(self.to_world(self.canvas.canvasy(screen_y)))
        self.canvas.itemconfig(self.mouse_coords_text, text=f"X: {real_x}, Y: {real_y}  |  Zoom: {int(self.current_scale * 100)}%")
        self.canvas.coords(self.mouse_coords_text, self.canvas.canvasx(10), self.canvas.canvasy(10))
        self.canvas.tag_raise(self.mouse_coords_text)

    def on_click(self, event):
        self.canvas.focus_set()
        self.drag_start_x, self.drag_start_y = self.canvas.canvasx(event.x), self.canvas.canvasy(event.y)
        self.click_real_x, self.click_real_y = self.to_world(self.drag_start_x), self.to_world(self.drag_start_y)
        
        overlapping = self.canvas.find_overlapping(self.drag_start_x, self.drag_start_y, self.drag_start_x+1, self.drag_start_y+1)
        obs_id = next((i for i in overlapping if i != self.mouse_coords_text and any(o['id'] == i for o in self.obstacles)), None)

        # Shift + Click: Editar Dimensiones
        if event.state & 0x0001 and obs_id:
            obs = self.get_obstacle_by_id(obs_id)
            if obs and (dims := create_obstacle_dialog(obs['x'], obs['y'])): 
                self.update_obstacle_dimensions(obs, dims[0] + dims[2]/2, dims[1] + dims[3]/2, dims[2], dims[3])
            return

        # Ctrl + Click: Borrar
        if event.state & 0x0004 and obs_id: return self.delete_obstacle(obs_id)

        # Alt + Click: Duplicar (Soporte para Alt estandar y Alt_L en algunos sistemas)
        # 0x0008 (Mod1) | 0x20000 (Alt Mask frecuente en Linux/Windows)
        if (event.state & 0x0008 or event.state & 0x20000) and obs_id:
            return self.duplicate_obstacle(obs_id)
        
        if obs_id:
            self.drag_mode = "move"
            self.selected_item = self.get_obstacle_by_id(obs_id)
            for o in self.obstacles: self.canvas.itemconfig(o['id'], fill="#cccccc")
            self.canvas.itemconfig(obs_id, fill="#aaaaaa")
            self.offset_x = self.drag_start_x - self.to_screen(self.selected_item['x'])
            self.offset_y = self.drag_start_y - self.to_screen(self.selected_item['y'])
        else:
            self.drag_mode = "wait"
            self.current_rect = None
            self.selected_item = None
            for o in self.obstacles: self.canvas.itemconfig(o['id'], fill="#cccccc")

    def duplicate_obstacle(self, obs_id):
        obs = self.get_obstacle_by_id(obs_id)
        if obs:
            # Creamos una copia desplazada 20 unidades
            new_x = obs['x'] + 20
            new_y = obs['y'] + 20
            self.add_obstacle(new_x, new_y, obs['w'], obs['h'], obs['angle'])

    def rotate_selection(self, delta_angle):
        if self.selected_item:
            self.selected_item['angle'] = (self.selected_item['angle'] + delta_angle) % 360
            self.refresh_obstacle_visual(self.selected_item)

    def on_drag(self, event):
        self.on_mouse_move(event)
        vis_x, vis_y = self.canvas.canvasx(event.x), self.canvas.canvasy(event.y)
 
        if self.drag_mode == "wait":
            if ((self.drag_start_x - vis_x)**2 + (self.drag_start_y - vis_y)**2)**0.5 > 5:
                self.drag_mode = "create"
                self.current_rect = self.canvas.create_rectangle(self.drag_start_x, self.drag_start_y, vis_x, vis_y, outline="blue", width=2, tags="temp")

        if self.drag_mode == "create" and self.current_rect: 
            self.canvas.coords(self.current_rect, self.drag_start_x, self.drag_start_y, vis_x, vis_y)
            
        elif self.drag_mode == "move" and self.selected_item: 
            self.selected_item['x'] = self.to_world(vis_x - self.offset_x)
            self.selected_item['y'] = self.to_world(vis_y - self.offset_y)
            self.refresh_obstacle_visual(self.selected_item)

    def on_release(self, event):
        vis_x, vis_y = self.canvas.canvasx(event.x), self.canvas.canvasy(event.y)

        if self.drag_mode == "create":
            if self.current_rect: self.canvas.delete(self.current_rect); self.current_rect = None
            real_end_x, real_end_y = self.to_world(vis_x), self.to_world(vis_y)
            w, h = abs(self.click_real_x - real_end_x), abs(self.click_real_y - real_end_y)
            
            if w > 1 and h > 1:
                cx = min(self.click_real_x, real_end_x) + w/2
                cy = min(self.click_real_y, real_end_y) + h/2
                self.add_obstacle(cx, cy, w, h)

        elif self.drag_mode == "wait":
            if dims := create_obstacle_dialog(self.click_real_x, self.click_real_y):
                self.add_obstacle(dims[0] + dims[2]/2, dims[1] + dims[3]/2, dims[2], dims[3])
            
        elif self.drag_mode == "move" and self.selected_item:
            self.canvas.itemconfig(self.selected_item['id'], fill="#aaaaaa")
            
        self.drag_mode = None
        self.canvas.tag_raise(self.mouse_coords_text)

    def add_obstacle(self, cx, cy, w, h, angle=0):
        poly_id = self.canvas.create_polygon(self.get_poly_coords(cx, cy, w, h, angle), fill="#cccccc", outline="black", width=2, tags="obstacle")
        text_id = self.canvas.create_text(self.to_screen(cx), self.to_screen(cy), text=f"{int(w)}x{int(h)}", tags="obstacle")
        obs = {'id': poly_id, 'text_id': text_id, 'x': cx, 'y': cy, 'w': w, 'h': h, 'angle': angle}
        self.obstacles.append(obs)
        self.canvas.tag_raise(self.mouse_coords_text)
        return obs

    def refresh_obstacle_visual(self, obs):
        self.canvas.coords(obs['id'], *self.get_poly_coords(obs['x'], obs['y'], obs['w'], obs['h'], obs['angle']))
        self.canvas.coords(obs['text_id'], self.to_screen(obs['x']), self.to_screen(obs['y']))

    def update_obstacle_dimensions(self, obs, new_cx, new_cy, new_w, new_h):
        obs['x'], obs['y'], obs['w'], obs['h'] = new_cx, new_cy, new_w, new_h
        self.refresh_obstacle_visual(obs)
        self.canvas.itemconfig(obs['text_id'], text=f"{int(new_w)}x{int(new_h)}")

    def delete_obstacle(self, obs_id):
        obs = self.get_obstacle_by_id(obs_id)
        if obs:
            self.canvas.delete(obs['id'])
            self.canvas.delete(obs['text_id'])
            self.obstacles.remove(obs)
            if self.selected_item == obs: self.selected_item = None

    def get_obstacle_by_id(self, rect_id):
        return next((obs for obs in self.obstacles if obs['id'] == rect_id), None)

    def clear_all(self):
        self.canvas.delete("all")
        self.obstacles = []
        self.selected_item = None
        self.mouse_coords_text = self.canvas.create_text(self.canvas.canvasx(10), self.canvas.canvasy(10), text="X: 0, Y: 0", anchor="nw", fill="red", font=("Arial", 10, "bold"))