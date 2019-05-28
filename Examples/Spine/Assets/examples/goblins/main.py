from kivy.app import App
from kivy.clock import Clock
from kivy.uix.widget import Widget
from os.path import join

from spinekivy.skeletonrenderer import SkeletonRenderer
from spinekivy.sprite import Sprite


class MainScreen(Widget):

    def __init__(self, **kwargs):
        super(MainScreen, self).__init__(**kwargs)
        self.skeleton_renderer = renderer = SkeletonRenderer()
        renderer.scale = 1.0
        renderer.load(join('assets', 'goblins'))
        renderer.skeleton.set_skin_by_name('goblin')
        renderer.skeleton.x = 320
        renderer.skeleton.y = 100
        renderer.sprites = [Sprite() for _ in renderer.skeleton.slots]
        renderer.state.set_animation_by_name(0, 'walk', True)
        for sprite in renderer.sprites:
            self.canvas.add(sprite)
        renderer.update(0)

    def update(self, dt):
        self.skeleton_renderer.update(dt)


class GoblinsApp(App):

    def build(self):
        return MainScreen()

    def on_start(self):
        super(GoblinsApp, self).on_start()
        Clock.schedule_interval(self.root.update, 0)

    def on_stop(self):
        super(GoblinsApp, self).on_stop()
        Clock.unschedule(self.root.update)


if __name__ == '__main__':
    GoblinsApp().run()
