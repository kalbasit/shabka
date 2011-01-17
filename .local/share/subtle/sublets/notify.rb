# Notify sublet file
# Created with sur-0.2.168
require "ffi"

# DBus interface
class DBus # {{{
  extend FFI::Library

  ffi_lib("libdbus-1")

  # DBus name
  DBUS_NAME  = "org.freedesktop.Notifications"

  # DBus path
  DBUS_PATH  = "/org/freedesktop/Notifications"

  # DBus interface
  DBUS_IFACE = "org.freedesktop.Notifications"

  # DBus connection
  attr_reader :connection

  # DBus connection file descriptor
  attr_reader :fd

  # DBus error
  attr_reader :error

  # Message id
  attr_reader :id

  # Icon for message
  attr_reader :icon

  # Summary of the notify
  attr_reader :summary

  # Body of the notify
  attr_reader :body

  # Message timeout
  attr_reader :timeout

  # DBusType {{{
  DBusBusType = enum(
  [
    :bus_session, 0,
    :bus_system,
    :bus_starter
  ]) # }}}

  # DBusOwnerFlags {{{
  DBusOwnerFlags = enum(
  [
    :flag_allow,    0x1,
    :flag_replace,  0x2,
    :flag_no_queue, 0x4
  ]) # }}}

  # DBusRequestReply {{{
  DBusRequestReply = enum(
  [
    :reply_primary,  1,
    :reply_in_queue, 2,
    :reply_exists,   3,
    :reply_owner,    4
  ]) # }}}

  # DBusType {{{
  DBusType = enum(
  [
    :type_int32,  "i".ord,
    :type_uint32, "u".ord,
    :type_string, "s".ord,
    :type_array,  "a".ord
  ]) # }}}

  # DBusError {{{
  class DBusError < FFI::Struct
    layout(
      :name,    :string,
      :message, :string,
      :dummy,   :char    #< Place holder
    )
  end # }}}

  # DBusMessageIter {{{
  class DBusMessageIter < FFI::Struct
    layout(
      :dummy1,  :pointer,
      :dummy2,  :pointer,
      :dummy3,  :uint32,
      :dummy4,  :int,
      :dummy5,  :int,
      :dummy6,  :int,
      :dummy7,  :int,
      :dummy8,  :int,
      :dummy9,  :int,
      :dummy10, :int,
      :dummy11, :int,
      :pad1,    :int,
      :pad2,    :int,
      :pad3,    :pointer
    )
  end # }}}

  ## initialize {{{
  # Initialize the class
  #
  # @return [Object] New #DBus object
  ###

  def initialize
    # Init error
    perror = FFI::MemoryPointer.new(:pointer)
    @error = DBusError.new(perror)

    dbus_error_init(@error) #< Init error

    # Create connection and replace current owner
    @connection = dbus_bus_get(:bus_session, @error)

    raise "Couldn't connect to dbus" if(@connection.nil?)

    reply = dbus_bus_request_name(@connection, DBUS_NAME,
      :flag_replace, @error)

    if(DBusRequestReply[:reply_primary] != reply)
      puts "Failed requesting ownership of `#{DBUS_NAME}': %s" % [
        case(reply)
          when 2 then "Service has been placed in queue"
          when 3 then "Service is already in queue"
          when 4 then "Service is already primary owner"
        end
      ]
    end

    # Get socket file descriptor
    pfd = FFI::MemoryPointer.new(:int)

    dbus_connection_get_unix_fd(@connection, pfd)

    @fd = pfd.null? ? 0 : pfd.read_int

    # Add message filter
    dbus_bus_add_match(@connection,
      "path='#{DBUS_PATH}', interface='#{DBUS_IFACE}'", @error)
    dbus_connection_flush(@connection)
    fetch

    @id = 0 #< Init counter
  end # }}}

  ## fetch {{{
  # Fetch data from connection
  #
  # @return [Bool] Whether fetch was successful
  ##

  def fetch
    ret = false

    dbus_connection_read_write(@connection, 0)

    mesg = dbus_connection_pop_message(@connection)

    # Check whether message is of desired type
    unless(mesg.null?)
      # UINT32 org.freedesktop.Notifications.Notify (STRING app_name,
      #   UINT32 replaces_id, STRING app_icon, STRING summary,
      #   STRING body, ARRAY actions, DICT hints, INT32 expire_timeout);
      if(dbus_message_is_method_call(mesg, DBUS_IFACE, "Notify"))
        @id += 1

        # Create iter
        iter  = DBusMessageIter.new
        piter = iter.pointer
        dbus_message_iter_init(mesg, piter)

        # Collect data from message
        name     = get_string(piter)
        replace  = get_fixnum(piter)
        @icon    = get_string(piter)
        @summary = get_string(piter)
        @body    = get_string(piter)

        # Ignore array and hints
        dbus_message_iter_next(piter)
        dbus_message_iter_next(piter)

        @timeout = get_fixnum(piter)

        # Create reply
        reply = dbus_message_new_method_return(mesg)
        pid   = FFI::MemoryPointer.new(:uint32)
        pid.write_int(@id)

        # Create iter
        iter  = DBusMessageIter.new
        piter = iter.pointer

        # Append data to iter
        dbus_message_iter_init_append(reply, piter)
        dbus_message_iter_append_basic(piter, :type_uint32, pid)
        dbus_connection_send(@connection, reply, nil)
        dbus_connection_flush(@connection)
        dbus_message_unref(reply)

        dbus_message_unref(mesg)

        ret = true
      # void org.freedesktop.Notifications.GetServerInformation (
      #   out STRING name, out STRING vendor, out STRING version);
      elsif(dbus_message_is_method_call(mesg, DBUS_IFACE,
          "GetServerInformation"))
        puts "GetServerInformation"

        # Create reply
        reply    = dbus_message_new_method_return(mesg)
        pname    = str2ptr("subtle")
        pvendor  = str2ptr("http://subtle.subforge.org")
        pversion = str2ptr("0.0")
        pspec    = str2ptr("0.9")

        # Create iter
        iter  = DBusMessageIter.new
        piter = iter.pointer

        dbus_message_iter_init_append(reply, piter)
        dbus_message_iter_append_basic(piter, :type_string, pname)
        dbus_message_iter_append_basic(piter, :type_string, pvendor)
        dbus_message_iter_append_basic(piter, :type_string, pversion)
        dbus_message_iter_append_basic(piter, :type_string, pspec)
        dbus_connection_send(@connection, reply, nil)
        dbus_connection_flush(@connection)
        dbus_message_unref(reply)

        ret = false
      # STRING_ARRAY org.freedesktop.Notifications.GetCapabilities (void);
      elsif(dbus_message_is_method_call(mesg, DBUS_IFACE, "GetCapabilities"))
        puts "GetCapabilities"

        # Create reply
        reply = dbus_message_new_method_return(mesg)

        # Create iters
        iter  = DBusMessageIter.new
        piter = iter.pointer

        dataiter  = DBusMessageIter.new
        pdataiter = dataiter.pointer

        # Create array
        pbody = str2ptr("body")

        dbus_message_iter_init_append(reply, piter)
        dbus_message_iter_open_container(piter, :type_array,
          DBusType[:type_string].chr, pdataiter)
        dbus_message_iter_append_basic(pdataiter, :type_string, pbody)
        dbus_message_iter_close_container(piter, pdataiter)
        dbus_connection_send(@connection, reply, nil)
        dbus_connection_flush(@connection)
        dbus_message_unref(reply)

        ret = false
      else
        fetch #< Repeat until we get something reasonable
      end
    end

    ret
  end # }}}

  ## finalize {{{
  # Finalize object
  ##

  def finalize
    dbus_connection_unref(@connection)
  end # }}}

  private

  # str2ptr {{{
  def str2ptr(str)
    value = FFI::MemoryPointer.from_string(str)
    ptr   = FFI::MemoryPointer.new(:pointer)

    ptr.put_pointer(0, value)

    ptr
  end # }}}

  # get_fixnum {{{
  def get_fixnum(iter)
    ret = 0

    begin
      pvalue = FFI::MemoryPointer.new(:int)

      dbus_message_iter_get_basic(iter, pvalue)
      dbus_message_iter_next(iter)

      ret = pvalue.null? ? 0 : pvalue.read_int
    rescue
      ret = 0
    end

    ret
  end # }}}

  # get_string {{{
  def get_string(iter)
    ret = ""

    # Get string part from iter
    begin
      pvalue = FFI::MemoryPointer.new(:pointer)

      dbus_message_iter_get_basic(iter, pvalue)
      dbus_message_iter_next(iter)

      svalue = pvalue.read_pointer

      ret = svalue.null? ? "" : svalue.read_string
    rescue
      ret = ""
    end

    ret
  end # }}}

  ## dbus_error_init {{{
  # Init error struct
  #
  # @param [Pointer, #write]  error  A #DBusError
  ##

  attach_function(:dbus_error_init,
    :dbus_error_init, [ :pointer ], :void
  ) # }}}

  ## dbus_bus_get {{{
  # Connect to bus daemon and register client
  #
  # @param [Fixnum,  #read]  type   A #DBusBusType
  # @param [Pointer, #read]  error  A #DBusError
  #
  # @return [Pointer]  A #DBusConnection
  ##

  attach_function(:dbus_bus_get,
    :dbus_bus_get, [ DBusBusType, :pointer ], :pointer
  ) # }}}

  ## dbus_bus_request_name {{{
  # Ask bus to assign specific name
  #
  # @param [Pointer, #read]  connection  A #DBusConnection
  # @param [String,  #read]  name        Name to request
  # @param [Fixnum,  #read]  flags       Flags
  # @param [Pointer, #read]  error       A #DBusError
  #
  # @return [Fixnum] Result code, -1 if error is set
  ##

  attach_function(:dbus_bus_request_name,
    :dbus_bus_request_name, [ :pointer, :string, DBusOwnerFlags, :pointer ],
    :int
  ) # }}}

  ## dbus_bus_add_match {{{
  # Add rule for messages to receive
  #
  # @param [Pointer, #read]   connection  A #DBusConnection
  # @param [String,  #read]   rule        Matching rule
  # @param [Poiner,  #write]  error       A #DBusError
  ##

  attach_function(:dbus_bus_add_match,
    :dbus_bus_add_match, [ :pointer, :string, :pointer ], :void
  ) # }}}

  ## dbus_connection_read_write {{{
  # Check connection for data
  #
  # @param [Pointer, #read]  connection            A #DBusConnection
  # @param [Fixnum,  #read]  timeout_milliseconds  Timeout in milliseconds
  #
  # @return [Bool]  Whether data is available
  ##

  attach_function(:dbus_connection_read_write,
    :dbus_connection_read_write, [ :pointer, :int ], :bool
  ) # }}}

  ## dbus_connection_pop_message {{{
  # Pop message from connection
  #
  # @param [Pointer, #read]  connection  A #DBusConnection
  #
  # @return [Bool]  Whether data is available
  ##

  attach_function(:dbus_connection_pop_message,
    :dbus_connection_pop_message, [ :pointer ], :pointer
  ) # }}}

  ## dbus_connection_get_unix_fd {{{
  # Get dbus connection socket fd
  #
  # @param [Pointer, #read]   connection  A #DBusConnection
  # @param [Pointer,  #write]  fd          Socket file descriptor
  #
  # @return [Bool]  Whether this was successful
  ##

  attach_function(:dbus_connection_get_unix_fd,
    :dbus_connection_get_unix_fd, [ :pointer, :pointer ], :bool
  ) # }}}

  ## dbus_connection_send {{{
  # Adds a message to the outgoing message queue
  #
  # @param [Pointer, #read]   connection  A #DBusConnection
  # @param [Pointer, #read]   message     A #DBusMessage
  # @param [Pointer, #write]  serial      Message serial
  #
  # @return [Bool]  Whether this was successful
  ##

  attach_function(:dbus_connection_send,
    :dbus_connection_send, [ :pointer, :pointer, :pointer ], :bool
  ) # }}}

  ## dbus_connection_flush {{{
  # Flush data on connection
  #
  # @param [Pointer, #read]  connection  A #DBusConnection
  ##

  attach_function(:dbus_connection_flush,
    :dbus_connection_flush, [ :pointer ], :void
  ) # }}}

  ## dbus_connection_unref {{{
  # Unreference connection
  #
  # @param [Pointer, #read]  connection  A #DBusConnection
  ##

  attach_function(:dbus_connection_unref,
    :dbus_connection_unref, [ :pointer ], :void
  ) # }}}

  ## dbus_message_new_method_return {{{
  # Construct a reply to a method call
  #
  # @param [Pointer, #read]  message  A #DBusMessage
  #
  # @return [Pointer] A #DbusMessage
  ##

  attach_function(:dbus_message_new_method_return,
    :dbus_message_new_method_return, [ :pointer ], :pointer
  ) # }}}

  ## dbus_message_iter_init {{{
  # Init message iter
  #
  # @param [Pointer, #read]  message  A #DBusMessage
  # @param [Pointer, #read]  iter     A #DBusMessageIter
  #
  # @return [Bool]  Whether init succeeded
  ##

  attach_function(:dbus_message_iter_init,
    :dbus_message_iter_init, [ :pointer, :pointer ], :bool
  ) # }}}

  ## dbus_message_iter_init_append {{{
  # Initializes a DBusMessageIter for appending arguments
  #
  # @param [Pointer, #read]  message  A #DBusMessage
  # @param [Pointer, #read]  iter     A #DBusMessageIter
  ##

  attach_function(:dbus_message_iter_init_append,
    :dbus_message_iter_init_append, [ :pointer, :pointer ], :void
  ) # }}}

  ## dbus_message_iter_get_arg_type {{{
  # Get the argument type of the message iter
  #
  # @param [Pointer, #read]  iter  A #DBusMessageIter
  #
  # @return [Fixnum] Argument type of iter
  ##

  attach_function(:dbus_message_iter_get_arg_type,
    :dbus_message_iter_get_arg_type, [ :pointer ], :int
  ) # }}}

  ## dbus_message_iter_next {{{
  # Get next iter
  #
  # @param [Pointer, #read]  iter  A #DBusMessageIter
  #
  # @return [Bool]  Whether next succeeded
  ##

  attach_function(:dbus_message_iter_next,
    :dbus_message_iter_next, [ :pointer ], :bool
  ) # }}}

  ## dbus_message_iter_append_basic {{{
  # Appends a basic-typed value to the message
  #
  # @param [Pointer, #read]  iter   A #DBusMessageIter
  # @param [Fixnum,  #read]  type   A #DBusType
  # @param [Pointer, #read]  value  Address of the value
  #
  # @return [Bool] Whether append succeeded
  ##

  attach_function(:dbus_message_iter_append_basic,
    :dbus_message_iter_append_basic, [ :pointer, DBusType, :pointer ], :bool
  ) # }}}

  ## dbus_message_iter_append_fixed_array {{{
  # Appends a block of fixed-length values to an array
  #
  # @param [Pointer, #read]  iter        A #DBusMessageIter
  # @param [Fixnum,  #read]  type        A #DBusType
  # @param [Pointer, #read]  value       Address of the array
  # @param [Fixnum,  #read]  n_elements  Number of elements to append
  #
  # @return [Bool] Whether append succeeded
  ##

  attach_function(:dbus_message_iter_append_fixed_array,
    :dbus_message_iter_append_fixed_array,
    [ :pointer, DBusType, :pointer, :int ], :bool
  ) # }}}

  ## dbus_message_iter_get_basic {{{
  # Get basic value from message
  #
  # @param [Pointer, #read]  message  A #DBusMessage
  # @param [Pointer, #read]  value    A value
  #
  # @return [Bool]  Whether get value succeeded
  ##

  attach_function(:dbus_message_iter_get_basic,
    :dbus_message_iter_get_basic, [ :pointer, :pointer ], :bool
  ) # }}}

  ## dbus_message_iter_open_container {{{
  # Appends a container-typed value to the message
  #
  # @param [Pointer, #read]  iter                 A #DBusMessageIter
  # @param [Fixnum,  #read]  type                 A #DBusType
  # @param [String,  #read]  contained_signature  Type of container contents
  # @param [Pointer, #read]  sub                  A #DBusMessageIter
  #
  # @return [Bool] Whether get value succeeded
  ##

  attach_function(:dbus_message_iter_open_container,
    :dbus_message_iter_open_container,
    [ :pointer, DBusType, :string, :pointer ], :bool
  ) # }}}

  ## dbus_message_iter_close_container {{{
  # Closes a container-typed value to the message
  #
  # @param [Pointer, #read]  iter                 A #DBusMessageIter
  # @param [Pointer, #read]  sub                  A #DBusMessageIter
  #
  # @return [Bool] Whether get value succeeded
  ##

  attach_function(:dbus_message_iter_close_container,
    :dbus_message_iter_close_container, [ :pointer, :pointer ], :bool
  ) # }}}

  ## dbus_message_is_method_call {{{
  # Check whether the mssage is a method call of given type
  #
  # @param [Pointer, #read]  message    A #DBusMessage
  # @param [String,  #read]  interface  Interface name
  # @param [String,  #read]  method     Method name
  #
  # @return [Bool] Whether method call is of given type
  ##

  attach_function(:dbus_message_is_method_call,
    :dbus_message_is_method_call, [ :pointer, :string, :string ], :bool
  ) # }}}

  ## dbus_message_unref {{{
  # Unreference message
  #
  # @param [Pointer, #read]  message  A #DBusMessage
  ##

  attach_function(:dbus_message_unref,
    :dbus_message_unref, [ :pointer ], :void
  ) # }}}
end # }}}

configure :notify do |s| # {{{
  s.interval = 9999
  s.messages = []
  s.dbus     = DBus.new

  # Colors
  colors = Subtlext::Subtle.colors

  s.colors = {
    :focus => colors[:focus_fg],
    :text  => colors[:panel_fg],
    :panel => colors[:panel_bg]
  }

  # Icon
  s.icon = Subtlext::Icon.new("info.xbm")

  # Window
  s.win = Subtlext::Window.new(:x => 0, :y => 0, :width => 1, :height => 1) do |w|
    w.name        = "Sublet notify"
    w.foreground  = s.colors[:text]
    w.background  = s.colors[:panel]
    w.border_size = 0
  end


  # Watch socket
  s.watch(s.dbus.fd)
end # }}}

on :run do |s| # {{{
  # Just idle
  s.interval = 9999
  s.data     = s.icon.to_s
end # }}}

on :watch do |s| # {{{
  # Fetch messages
  if(s.dbus.fetch)
    s.messages << s.dbus.summary

    s.data = "%s%s" % [ s.colors[:focus], s.icon ]
  end
end # }}}

on :mouse_over do |s| # {{{
  # Show and print messages
  if(0 < s.messages.size)
    x      = 0
    y      = 0
    width  = 1
    height = 5

    # Write each message and calculate window width
    s.messages.each_index do |i|
      size    = s.win.write(2, 15 * (i + 1), s.messages[i][0..50])
      width   = size if(size > width) #< Get biggest
      height += 15
    end

    # Orientation
    screen_geom = Subtlext::Screen[0].geometry
    sublet_geom = s.geometry

    if(sublet_geom.x + width > screen_geom.x + screen_geom.width)
      x = screen_geom.x + screen_geom.width - width
    else
      x = sublet_geom.x
    end

    if(sublet_geom.y + height > screen_geom.y + screen_geom.height)
      y = screen_geom.y + screen_geom.height - height
    else
      y = sublet_geom.y + sublet_geom.height + 1
    end

    s.win.geometry = [ x, y, width, height ]

    s.win.show
  end
end # }}}

on :mouse_out do |s| # {{{
  # Hide window
  if(0 < s.messages.size)
    s.win.hide
    s.messages = []
    s.data     = s.icon.to_s
  end
end # }}}

on :exit do |s| # {{{
  # Tidy up
  s.win.kill unless(s.win.nil?)
  s.dbus.finalize
end # }}}
