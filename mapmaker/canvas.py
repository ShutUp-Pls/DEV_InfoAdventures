import tkinter as tk
from tkinter import simpledialog

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
        
        self.offset_x = 0
        self.offset_y = 0
        self.start_x = 0
        self.start_y = 0
        self.last_mouse_x = 0
        self.last_mouse_y = 0
        
        self.keys = {
            "w": False, "a": False, "s": False, "d": False, 
            "shift_l": False, "shift_r": False
        }
        
        self.camera_speed = 5
        self.sprint_speed = 15
        self.acc_x = 0.0
        self.acc_y = 0.0
        
        self.canvas = tk.Canvas(self, width=self.WIDTH, height=self.HEIGHT, bg="white", cursor="cross")
        self.canvas.pack(padx=10, pady=10, expand=True, fill=tk.BOTH)
        self.canvas.config(scrollregion=(-5000, -5000, 5000, 5000), xscrollincrement=1, yscrollincrement=1)
        
        self.mouse_coords_text = self.canvas.create_text(
            10, 10, text="X: 0, Y: 0  |  Zoom: 100%",
            anchor="nw", fill="red", font=("Arial", 10, "bold")
        )
        
        events = [
            ("<Button-1>", self.on_click),
            ("<B1-Motion>", self.on_drag),
            ("<ButtonRelease-1>", self.on_release),
            ("<Motion>", self.on_mouse_move),
            ("<KeyPress>", self.on_key_press),
            ("<KeyRelease>", self.on_key_release),
            ("<minus>", self.zoom_out),
            ("<KP_Subtract>", self.zoom_out),
            ("<plus>", self.zoom_in),
            ("<KP_Add>", self.zoom_in)
        ]
        
        for event, func in events: self.canvas.bind(event, func)
        
        self.canvas.focus_set()
        self.update_camera_loop()

    def to_world(self, screen_val):
        return screen_val / self.current_scale

    def to_screen(self, world_val):
        return world_val * self.current_scale

    def zoom_in(self, event):
        self.apply_zoom(1.1)

    def zoom_out(self, event):
        self.apply_zoom(0.9)

    def apply_zoom(self, factor):
        off_x = self.canvas.canvasx(self.last_mouse_x)
        off_y = self.canvas.canvasy(self.last_mouse_y)
        self.canvas.scale("obstacle", 0, 0, factor, factor)

        self.current_scale *= factor

        new_off_x = off_x * factor
        new_off_y = off_y * factor

        diff_x = new_off_x - off_x
        diff_y = new_off_y - off_y

        self.canvas.xview_scroll(int(diff_x), "units")
        self.canvas.yview_scroll(int(diff_y), "units")

        # Actualizamos HUD
        self.update_hud(self.last_mouse_x, self.last_mouse_y)

    def on_key_press(self, event):
        key = event.keysym.lower()
        if key in self.keys: self.keys[key] = True

    def on_key_release(self, event):
        key = event.keysym.lower()
        if key in self.keys: self.keys[key] = False

    def update_camera_loop(self):
        target_dx = 0
        target_dy = 0
        current_speed = self.sprint_speed if (self.keys["shift_l"] or self.keys["shift_r"]) else self.camera_speed
        
        if self.keys["a"]: target_dx = -current_speed
        if self.keys["d"]: target_dx = current_speed
        if self.keys["w"]: target_dy = -current_speed
        if self.keys["s"]: target_dy = current_speed
        
        self.acc_x += target_dx
        self.acc_y += target_dy
        
        scroll_x = int(self.acc_x)
        scroll_y = int(self.acc_y)
        
        if scroll_x != 0 or scroll_y != 0:
            self.canvas.xview_scroll(scroll_x, "units")
            self.canvas.yview_scroll(scroll_y, "units")
            self.acc_x -= scroll_x
            self.acc_y -= scroll_y
            self.update_hud(self.last_mouse_x, self.last_mouse_y)
            
        self.after(16, self.update_camera_loop)

    def on_mouse_move(self, event):
        self.last_mouse_x = event.x
        self.last_mouse_y = event.y
        self.update_hud(event.x, event.y)

    def update_hud(self, screen_x, screen_y):
        cx_visual = self.canvas.canvasx(screen_x)
        cy_visual = self.canvas.canvasy(screen_y)

        real_x = int(self.to_world(cx_visual))
        real_y = int(self.to_world(cy_visual))

        zoom_pct = int(self.current_scale * 100)

        self.canvas.itemconfig(
            self.mouse_coords_text, 
            text=f"X: {real_x}, Y: {real_y}  |  Zoom: {zoom_pct}%"
        )

        self.canvas.coords(self.mouse_coords_text, self.canvas.canvasx(10), self.canvas.canvasy(10))
        self.canvas.tag_raise(self.mouse_coords_text)

    def on_click(self, event):
        self.canvas.focus_set()
        self.drag_start_x = self.canvas.canvasx(event.x)
        self.drag_start_y = self.canvas.canvasy(event.y)

        self.click_real_x = self.to_world(self.drag_start_x)
        self.click_real_y = self.to_world(self.drag_start_y)
        
        self.last_mouse_x = event.x
        self.last_mouse_y = event.y

        overlapping = self.canvas.find_overlapping(self.drag_start_x, self.drag_start_y, self.drag_start_x, self.drag_start_y)
        rect_id = next((i for i in overlapping if i != self.mouse_coords_text and any(obs['id'] == i for obs in self.obstacles)), None)

        if event.state & 0x0001 and rect_id:
            obs = self.get_obstacle_by_id(rect_id)
            if obs and (dims := create_obstacle_dialog(obs['x'], obs['y'])): 
                self.update_obstacle_dimensions(obs, *dims)
            return

        if event.state & 0x0004 and rect_id: return self.delete_obstacle(rect_id)
        
        if rect_id:
            self.drag_mode = "move"
            self.selected_item = self.get_obstacle_by_id(rect_id)

            coords = self.canvas.coords(rect_id)
            self.offset_x = self.drag_start_x - coords[0]
            self.offset_y = self.drag_start_y - coords[1]
            
            self.canvas.itemconfig(rect_id, fill="#aaaaaa")
        else:
            self.drag_mode = "wait"
            self.current_rect = None

    def on_drag(self, event):
        self.on_mouse_move(event)
        vis_x = self.canvas.canvasx(event.x)
        vis_y = self.canvas.canvasy(event.y)
 
        if self.drag_mode == "wait":
            dist = ((self.drag_start_x - vis_x)**2 + (self.drag_start_y - vis_y)**2)**0.5
            if dist > 5:
                self.drag_mode = "create"
                self.current_rect = self.canvas.create_rectangle(
                    self.drag_start_x, self.drag_start_y, vis_x, vis_y, 
                    outline="blue", width=2, tags="obstacle"
                )

        if self.drag_mode == "create": 
            if self.current_rect:
                self.canvas.coords(self.current_rect, self.drag_start_x, self.drag_start_y, vis_x, vis_y)
            
        elif self.drag_mode == "move" and self.selected_item: 
            new_x1 = vis_x - self.offset_x
            new_y1 = vis_y - self.offset_y
            
            coords = self.canvas.coords(self.selected_item['id'])
            w_vis = coords[2] - coords[0]
            h_vis = coords[3] - coords[1]
            
            self.canvas.coords(self.selected_item['id'], new_x1, new_y1, new_x1 + w_vis, new_y1 + h_vis)
            self.canvas.coords(self.selected_item['text_id'], new_x1 + w_vis/2, new_y1 + h_vis/2)

    def on_release(self, event):
        vis_x = self.canvas.canvasx(event.x)
        vis_y = self.canvas.canvasy(event.y)

        if self.drag_mode == "create":
            if self.current_rect:
                self.canvas.delete(self.current_rect)
                self.current_rect = None

            real_end_x = self.to_world(vis_x)
            real_end_y = self.to_world(vis_y)

            x = min(self.click_real_x, real_end_x)
            y = min(self.click_real_y, real_end_y)
            w = abs(self.click_real_x - real_end_x)
            h = abs(self.click_real_y - real_end_y)
            
            if w > 1 and h > 1:
                self.add_obstacle(x, y, w, h)

        elif self.drag_mode == "wait":
            dims = create_obstacle_dialog(self.click_real_x, self.click_real_y)
            if dims:
                self.add_obstacle(self.click_real_x, self.click_real_y, dims[2], dims[3])
            
        elif self.drag_mode == "move" and self.selected_item:
            self.canvas.itemconfig(self.selected_item['id'], fill="#cccccc")
            coords = self.canvas.coords(self.selected_item['id'])

            self.selected_item['x'] = self.to_world(coords[0])
            self.selected_item['y'] = self.to_world(coords[1])
            self.selected_item = None
            
        self.drag_mode = None
        self.canvas.tag_raise(self.mouse_coords_text)

    def add_obstacle(self, real_x, real_y, real_w, real_h):
        vis_x = self.to_screen(real_x)
        vis_y = self.to_screen(real_y)
        vis_w = self.to_screen(real_w)
        vis_h = self.to_screen(real_h)
        
        rect_id = self.canvas.create_rectangle(
            vis_x, vis_y, vis_x + vis_w, vis_y + vis_h, 
            fill="#cccccc", outline="black", width=2, tags="obstacle"
        )
        text_id = self.canvas.create_text(
            vis_x + vis_w/2, vis_y + vis_h/2, 
            text=f"{int(real_w)}x{int(real_h)}", tags="obstacle"
        )

        self.obstacles.append({
            'id': rect_id, 
            'text_id': text_id, 
            'x': real_x, 
            'y': real_y, 
            'w': real_w, 
            'h': real_h
        })
        self.canvas.tag_raise(self.mouse_coords_text)

    def move_obstacle(self, obs, new_real_x, new_real_y):
        vis_x = self.to_screen(new_real_x)
        vis_y = self.to_screen(new_real_y)
        vis_w = self.to_screen(obs['w'])
        vis_h = self.to_screen(obs['h'])
        
        obs['x'] = new_real_x
        obs['y'] = new_real_y
        
        self.canvas.coords(obs['id'], vis_x, vis_y, vis_x + vis_w, vis_y + vis_h)
        self.canvas.coords(obs['text_id'], vis_x + vis_w/2, vis_y + vis_h/2)

    def update_obstacle_dimensions(self, obs, new_real_x, new_real_y, new_real_w, new_real_h):
        obs['x'], obs['y'] = new_real_x, new_real_y
        obs['w'], obs['h'] = new_real_w, new_real_h

        vis_x = self.to_screen(new_real_x)
        vis_y = self.to_screen(new_real_y)
        vis_w = self.to_screen(new_real_w)
        vis_h = self.to_screen(new_real_h)
        
        self.canvas.coords(obs['id'], vis_x, vis_y, vis_x + vis_w, vis_y + vis_h)
        self.canvas.coords(obs['text_id'], vis_x + vis_w/2, vis_y + vis_h/2)
        self.canvas.itemconfig(obs['text_id'], text=f"{int(new_real_w)}x{int(new_real_h)}")
        self.canvas.tag_raise(self.mouse_coords_text)

    def delete_obstacle(self, rect_id):
        obs = self.get_obstacle_by_id(rect_id)
        if obs:
            self.canvas.delete(obs['id'])
            self.canvas.delete(obs['text_id'])
            self.obstacles.remove(obs)

    def get_obstacle_by_id(self, rect_id):
        return next((obs for obs in self.obstacles if obs['id'] == rect_id), None)

    def clear_all(self):
        self.canvas.delete("all")
        self.obstacles = []
        self.mouse_coords_text = self.canvas.create_text(
            self.canvas.canvasx(10), 
            self.canvas.canvasy(10), 
            text="X: 0, Y: 0", 
            anchor="nw", 
            fill="red", 
            font=("Arial", 10, "bold")
        )

if __name__ == "__main__":
    root = tk.Tk()
    root.title("Editor de Mapas")
    app = MapCanvas(root)
    app.pack(fill="both", expand=True)
    root.mainloop()