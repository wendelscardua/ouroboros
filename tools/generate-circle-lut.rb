#!/usr/bin/ruby
# frozen_string_literal: true

RADIUS = 0x68
X_CENTER = 0x80
Y_CENTER = 0x70

coords = (0...256).map do |i|
  fraction = 2 * Math::PI * i / 256.0
  x = X_CENTER + RADIUS * Math.sin(fraction)
  y = Y_CENTER + RADIUS * Math.cos(fraction)
  ["$#{x.round.to_s(16)}", "$#{y.round.to_s(16)}"]
end

x_bytes = coords.map(&:first).join(', ')
y_bytes = coords.map(&:last).join(', ')

puts "circle_x_lut: .byte #{x_bytes}"
puts "circle_y_lut: .byte #{y_bytes}"
