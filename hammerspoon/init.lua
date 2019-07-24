-- Hotkeys
hs.hotkey.bind({"alt", "command", "shift", "control"}, "e", function()
      hs.application.launchOrFocus("Emacs")
end)

hs.hotkey.bind({"ctrl", "alt", "shift", "command"}, "f", function()
      hs.application.launchOrFocus("Firefox Developer Edition")
end)

hs.hotkey.bind({"ctrl", "alt", "shift", "command"}, "u", function()
      hs.application.launchOrFocus("iTerm")
end)

-- window management
hs.loadSpoon("Lunette")

spoon.Lunette:bindHotkeys()
