# Writes the VX Ace map and MapInfos that tests/vxace_text.rs reads. Keep the expected text there in step with this file.
#
#   ruby generate.rb
#
# Stub classes stand in for the game's RPG classes: Marshal needs their names and instance variables only.

module RPG
  class Map; end
  class MapInfo; end
  class EventCommand; end

  class Event
    class Page; end
  end
end

def object(klass, ivars)
  obj = klass.allocate
  ivars.each { |name, value| obj.instance_variable_set("@#{name}", value) }
  obj
end

def command(code, parameters = [])
  object(RPG::EventCommand, code: code, indent: 0, parameters: parameters)
end

# Each message is its own event, since consecutive lines of one event are one translation entry.
def event(id, commands)
  object(RPG::Event, id: id, name: "EV00#{id}", x: id, y: id, pages: [
    object(RPG::Event::Page, list: commands + [command(0)])
  ])
end

tile = event(1, [command(401, ["\\nbtHello there."])])
tile_second = event(2, [command(401, ["\\nbltSecond line."])])
actor = event(3, [command(401, ["\\et[1]Hi there."])])
condition = event(4, [command(401, ["Take it.if(s[1])"])])
shop = event(5, [command(655, ["shop_talk_1 = \"Welcome.\""])])

choice = event(6, [command(102, [["Yes.", "No."], 1])])
misc = event(7, [command(320, [1, "Old name."])])
misc_two = event(8, [command(324, [1, "Other misc."])])
system = event(9, [command(356, ["System message."])])
credit = event(10, [command(405, ["Credit line."])])
# Code 101 starts a message box on XP only; on VX Ace it is ignored, and the line after it is a plain message.
start = event(11, [command(101, ["", 0, 0, 2]), command(401, ["After start."])])

map = object(RPG::Map, display_name: "Town", events: {
  1 => tile, 2 => tile_second, 3 => actor, 4 => condition, 5 => shop,
  6 => choice, 7 => misc, 8 => misc_two, 9 => system, 10 => credit, 11 => start
})
mapinfos = { 1 => object(RPG::MapInfo, name: "Town", order: 1, parent_id: 0) }

File.binwrite(File.join(__dir__, "Map001.rvdata2"), Marshal.dump(map))
File.binwrite(File.join(__dir__, "MapInfos.rvdata2"), Marshal.dump(mapinfos))
