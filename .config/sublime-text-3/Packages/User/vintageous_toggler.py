import sublime
import sublime_plugin

class ToggleVintageous(sublime_plugin.WindowCommand):
  def run(self):
    setts = sublime.load_settings('Preferences.sublime-settings')
    ignored = setts.get('ignored_packages')

    if 'Vintageous' in ignored:
      ignored.remove('Vintageous')
    else:
      ignored.append('Vintageous')

      setts.set('ignored_packages', ignored)
      sublime.save_settings('Preferences.sublime-settings')
