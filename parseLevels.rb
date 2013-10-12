# game data
# levels from: http://www.gamefaqs.com/gameboy/585643-boxxle/faqs/52416

# TODO not working properly, but doesn't seem to matter
def calcMaxRows(tiles)
	max = 0
	tiles.each do |row|
		if row.length > max 
			max = row.length
		end
	end
	return max
end

def isBox(c)
  	return (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
end

def isTarget(c)
  return c == '='
end

def isWall(c)
  return c == 'X'
end

def isPlayer(c)
  return c == '@'
end

=begin
curent desired output
    roomBuilder 
        [[1,1,1,1,1,0,0,0,0]                    -- map tiles
        ,[1,0,0,0,1,0,0,0,0]
        ,[1,0,0,0,1,0,1,1,1]
        ,[1,0,0,0,1,0,1,4,1]
        ,[1,1,1,0,1,1,1,4,1]
        ,[0,1,1,0,0,0,0,4,1]
        ,[0,1,0,0,0,1,0,0,1]
        ,[0,1,0,0,0,1,1,1,1]
        ,[0,1,1,1,1,1,0,0,0]
        ]
        [(Coord 2 2), (Coord 3 2), (Coord 2 3)] -- boxes
        [(Coord 7 3), (Coord 7 4), (Coord 7 5)] -- targets
        (Coord 1 1)                             -- start pos

=end
def prettyPrint(tiles, boxes, targets, startPos) 
	print "    ,roomBuilder\n"
	print "        ["
	x = 0
	y = 0
	tiles.each do |row|
		newRow = Array.new
		if y < tiles.length && y > 0
			print '        ,'
		end
		print '['
		x = 0
		row.each do |column|
			print column
			if x < row.length - 1
				print ','
			end
			x += 1
		end
		y += 1
		print "]\n"
	end
	print "        ] -- tiles\n"


	print "        ["
	x = 0
	boxes.each do |box|
		print "(Coord #{box[0]} #{box[1]})"
		if x < boxes.length - 1
			print ","
		end
		x += 1
	end
	print "] -- boxes\n"


	print "        ["
	x = 0
	targets.each do |target|
		print "(Coord #{target[0]} #{target[1]})"
		if x < targets.length - 1
			print ","
		end
		x += 1
	end
	print "] -- targets\n"

	print "        (Coord #{startPos[0]} #{startPos[1]}) -- start pos\n"
end

def parseLevel(level) 
	maxRows = calcMaxRows(level)
	tiles = Array.new
	boxes = Array.new
	targets = Array.new
	startPos = [1,1]
	x = 0
	y = 0
	level.each do |row|
		newRow = Array.new
		x = 0
		row.each do |column|
			if isWall column
				newRow << 1
			elsif isBox column
				boxes << [x, y]
				newRow << 0
			elsif isTarget column
				targets << [x, y]
				newRow << 0
			elsif isPlayer column
				startPos = [x, y]
				newRow << 0
			else
				newRow << 0
			end
			x += 1
		end
		if x < maxRows
			(x..maxRows).each do
				newRow << 0
			end
		end
		y += 1
		tiles << newRow
	end
	prettyPrint(tiles, boxes, targets, startPos)
end

level = Array.new
File.readlines('ROOM_DATA.txt').each do |line|
  	if line.start_with? '--'
  		level = Array.new
  	elsif line.strip == ''
  		if level.length > 0
  			parseLevel level
  		end
  	else
  		level << line.split("")
  	end
end