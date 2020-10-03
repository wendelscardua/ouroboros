#!/usr/bin/ruby
# frozen_string_literal: true

X_CENTER = 0x80
Y_CENTER = 0x78

%w[x y].each do |coord|
  puts ".define circle_lut_#{coord}_ptr " + (0..3).map { |i| "circle_#{i}_#{coord}_lut" }.join(', ')
  puts "circle_lut_#{coord}_ptr_h: .hibytes circle_lut_#{coord}_ptr"
  puts "circle_lut_#{coord}_ptr_l: .lobytes circle_lut_#{coord}_ptr"
end

[0x70, 0x60, 0x50, 0x40].each.with_index do |radius, index|
  coords = (0...256).map do |i|
    fraction = 2 * Math::PI * i / 256.0
    x = X_CENTER + radius * Math.sin(fraction)
    y = Y_CENTER + radius * Math.cos(fraction)
    ["$#{x.round.to_s(16)}", "$#{y.round.to_s(16)}"]
  end

  x_bytes = coords.map(&:first).join(', ')
  y_bytes = coords.map(&:last).join(', ')

  puts "circle_#{index}_x_lut: .byte #{x_bytes}"
  puts "circle_#{index}_y_lut: .byte #{y_bytes}"
end
