# Mpd sublet file
# Created with sur-0.1
require "socket"

# Pointer {{{
class Pointer
  attr_accessor :value

  def initialize(value = nil)
    @value = value
  end

  def to_s
    value.to_s
  end
end # }}}

configure :mpd do |s| # {{{
  # Icons
  s.icons = {
    :play    => Subtlext::Icon.new("play.xbm"),
    :pause   => Subtlext::Icon.new("pause.xbm"),
    :stop    => Subtlext::Icon.new("stop.xbm"),
    :prev    => Subtlext::Icon.new("prev.xbm"),
    :next    => Subtlext::Icon.new("next.xbm"),
    :note    => Subtlext::Icon.new("note.xbm"),
    :repeat  => Subtlext::Icon.new("repeat.xbm"),
    :random  => Subtlext::Icon.new("shuffle.xbm"),
    :update  => Subtlext::Icon.new("diskette.xbm")
  }

  # Options
  s.host          = s.config[:host]  || ENV["MPD_HOST"] || "localhost"
  s.port          = s.config[:port]  || ENV["MPD_PORT"] || 6600
  s.debug         = s.config[:debug] || false
  s.def_action    = s.config[:def_action]
  s.wheel_up      = s.config[:wheel_up]
  s.wheel_down    = s.config[:wheel_down]
  s.format_string = s.config[:format_string] || "%note%%artist% - %title%"
  s.interval      = 999

  # Sanitize actions
  valid = [ "play", "pause 0", "pause 1", "stop", "previous", "next", "stop" ]

  s.def_action = "next"     unless(valid.include?(s.def_action))
  s.wheel_up   = "next"     unless(valid.include?(s.wheel_up))
  s.wheel_down = "previous" unless(valid.include?(s.wheel_down))

  # Parse format string once
  fields = [ "%note%", "%artist%", "%album%", "%title%", "%track%", "%id%" ]

  s.format_values = {}

  s.format_string.gsub!(/%[^%]+%/) do |f|
    if(fields.include?(f))
      name = f.delete("%")

      if("%note%" == f)
        format_values[name] = self.icons[:note]
      else
        format_values[name] = Pointer.new
      end

      "%s"
    else
      ""
    end
  end

  # Modes
  s.repeat  = false
  s.shuffle = false
  s.update  = false

  # Connect
  if(s.connect(s.host, s.port))
    idle
    watch(self.socket) #< Start socket watching
  end
end # }}}

helper do |s| # {{{

  ## shutdown {{{
  # Shutdown connection
  ##

  def shutdown
    unwatch #< Stop watching socket

    self.socket = nil
    self.state  = :off

    update_status
  end # }}}

  ## connect {{{
  # Open connection to mpd
  # @param [String]  host  Hostname
  # @param [Fixnum]  port  Port
  # @return [Bool] Whether connection succeed
  ##

  def connect(host, port)
    begin
      self.socket = TCPSocket.new(host, port)

      # Handle SIGPIPE
      trap "PIPE" do
        shutdown
        puts "mpd signal: SIGPIPE" if(self.debug)
      end

      safe_read(1) #< Wait for mpd header
      update_status

      true
    rescue
      update_status

      false
    end
  end # }}}

  ## disconnect {{{
  # Send close and shutdown
  ###

  def disconnect
    unless(self.socket.nil?)
      safe_write("close")

      shutdown
    end
  end # }}}

  ## safe_read {{{
  # Read data from socket
  # @param [Fixnum]  timeout  Timeout in seconds
  # @return [String] Read data
  ##

  def safe_read(timeout = 0)
    line = ""

    begin
      sets = select([ self.socket ], nil, nil, timeout)
      line = self.socket.readline unless(sets.nil?) #< No nil is a socket hit

      puts "mpd read: %s" % [ line ] if(self.debug)
    rescue EOFError
      shutdown
      puts "mpd read: EOF" if(self.debug)
    rescue
      shutdown
    end

    line
  end # }}}

  ## safe_write {{{
  # Write dats to socket
  # @param [String]  str  String to write
  ##

  def safe_write(str)
    return if(str.empty?)

    begin
      self.socket.write("%s\n" % [ str ]) unless(self.socket.nil?)

      puts "mpd write: %s" % [ str ] if(self.debug)
    rescue
      shutdown
    end
  end # }}}

  ## idle {{{
  # Send idle command
  ##

  def idle
    safe_write("idle player options update")
  end # }}}

  ## noidle {{{
  # Send noidle command
  ###

  def noidle
    safe_write("noidle")
  end # }}}

  ## get_reply {{{
  # Send command and return reply as hash
  # @oaran [String]  command  Command to send
  # return [Hash] Data hash
  ###

  def get_reply(command)
    hash = {}

    begin
      safe_write(command)

      while
        line = safe_read(1)

        # Check response
        if(line.match(/^OK/))
          break
        elsif((match = line.match(/^ACK \[(.*)\] \{(.*)\} (.*)/)))
          s.state = :error

          puts "mpd error: %s" % [ match[3] ]

          safe_write("clearerror")

          break
        elsif((match = line.match(/^(\w+): (.+)$/)))
          hash[match[1].downcase] = match[2]
        end
      end
    rescue
      hash = {}
    end

    puts hash.inspect if(self.debug)

    hash
  end # }}}

  ## get_status {{{
  # Get mpd status
  # return [Hash]  Status hash
  ###

  def get_status
    unless(self.socket.nil?)
      status = get_reply("status")

      # Convert state
      self.state = case status["state"]
        when "play"  then :play
        when "pause" then :pause
        when "stop"  then :stop
        else :off
      end

      # Set modes
      self.repeat = (0 == status["repeat"].to_i) ? false : true
      self.random = (0 == status["random"].to_i) ? false : true
      self.update = !status["updating_db"].nil?

      puts "mpd status: %s" % [ status["state"] ] if(self.debug)

      status
    end
  end # }}}

  ## get_ok {{{
  # Get ok or error
  # @param [Fixnum]  timeout  Timeout in seconds
  # @return [Bool] Whether mpd return ok
  ##

  def get_ok(timeout = 0)
    unless(self.socket.nil?)
      line = safe_read(timeout)
      line = safe_read(timeout) if(line.match(/^changed/)) #< Skip changed message

      # Check result
      if(line.match(/^OK/))
        true
      elsif((match = line.match(/^ACK \[(.*)\] \{(.*)\} (.*)/)))
        puts "mpd error: %s" % [ match[3] ]

        safe_write("clearerror")

        true
      else
        # Probably non-recoverable
        puts "mpd error: expected ok - got: %s" % [ line ] if(self.debug)

        shutdown
      end
    end
  end # }}}

  ## update_status {{{
  # Update status and set data
  ##

  def update_status
    mesg  = "mpd not running"
    modes = ""
    icon  = :play

    unless(self.socket.nil?)
      get_status

      if(:play == self.state or :pause == self.state)
        song = get_reply("currentsong")

        # Select icon
        icon = case self.state
          when :play  then :pause
          when :pause then :play
        end

        # Sanity?
        self.format_values.each do |k, v|
          if(song.include?(k))
            self.format_values[k].value = song[k] || "n/a"
          end
        end

        # Assemble format
        mesg = self.format_string % self.format_values.values
      elsif(:stop == self.state)
        mesg = "mpd stopped"
        icon = :play
      end
    end

    # Mode
    modes << self.icons[:repeat] if(self.repeat)
    modes << self.icons[:random] if(self.random)
    modes << self.icons[:update] if(self.update)
    modes = " %s" % [ modes ] unless(modes.empty?)

    self.data = "%s%s%s%s%s %s" % [
      self.icons[icon], self.icons[:stop], self.icons[:prev], self.icons[:next],
      modes, mesg
    ]
  end # }}}
end # }}}

on :mouse_down do |s, x, y, b| # {{{
  if(s.socket.nil?)
    connect(s.host, s.port)
    update_status
    idle

    watch(self.socket)
  else
    noidle
    get_ok(1)

    # Send to socket
    safe_write(
      case b
        when 1
          case x
            when 0..14
              case s.state
                when :stop  then "play"
                when :pause then "pause 0"
                when :play  then "pause 1"
              end
            when 15..28 then "stop"
            when 29..42 then "previous"
            when 43..56 then "next"
            else s.def_action
          end
        when 4 then self.wheel_up
        when 5 then self.wheel_down
      end
    )
  end
end # }}}

on :watch do |s| # {{{
  get_ok(1)
  update_status
  idle
end # }}}
