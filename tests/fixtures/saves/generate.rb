# Writes the save fixtures that tests/save.rs reads. Keep the expected values there in step with this file.
#
#   ruby generate.rb
#
# Stub classes stand in for the game's own: Marshal only needs their names and instance variables, and the
# Marshal format is the same across Ruby versions.

Game_Map = Class.new
Game_Party = Class.new
Game_Actors = Class.new
Game_Actor = Class.new
Game_Switches = Class.new
Game_Variables = Class.new

def object(klass, ivars)
  obj = klass.allocate
  ivars.each { |name, value| obj.instance_variable_set("@#{name}", value) }
  obj
end

def actor(id, name, nickname, class_id, level, hp, mp, tp)
  object(Game_Actor, actor_id: id, name: name, nickname: nickname, class_id: class_id, level: level, hp: hp, mp: mp, tp: tp)
end

# VX Ace: a header stream holding the playtime in seconds, then one hash of the game's objects.
vxace_streams = [
  { playtime_s: 1234 },
  {
    map: object(Game_Map, map_id: 12),
    party: object(Game_Party, gold: 99, actors: [2, 1], items: { 1 => 3 }, weapons: { 4 => 1 }, armors: {}),
    actors: object(Game_Actors, data: [nil, actor(1, "Harold", "Hero", 3, 9, 40, 12, 5), actor(2, "Marsha", "", 4, 2, 1, 2, 3)]),
    switches: object(Game_Switches, data: [nil, true, false]),
    variables: object(Game_Variables, data: [nil, 5, 7]),
  },
]

# XP: one stream per object, the frame count first. The party holds its actors themselves, and Game_Actors
# repeats one of them, so the summary has to skip the duplicate.
harold = object(Game_Actor, actor_id: 1, name: "Harold", class_id: 3, level: 9, hp: 40, sp: 12)
marsha = object(Game_Actor, actor_id: 2, name: "Marsha", class_id: 4, level: 2, hp: 1, sp: 8)

xp_streams = [
  5400,
  object(Game_Map, map_id: 7),
  object(Game_Party, gold: 150, actors: [marsha, harold]),
  object(Game_Actors, data: [nil, harold]),
]

File.open(File.join(__dir__, "vxace.rvdata2"), "wb") do |file|
  vxace_streams.each { |stream| file.write(Marshal.dump(stream)) }
end

File.open(File.join(__dir__, "xp.rxdata"), "wb") do |file|
  xp_streams.each { |stream| file.write(Marshal.dump(stream)) }
end
