notify_cmd="/usr/bin/notify-send -i /usr/share/icons/Human/scalable/emblems/emblem-mail.svg"
if num>=1
  notify_summary="sup found #{num_inbox}/#{num} new message"
  if num>1
    notify_summary << "s"
  end
  notify_body = ''
  from_and_subj.each { |f,s| notify_body << "'#{f} : #{s}'" }
  system "#{notify_cmd} '#{notify_summary}' '#{notify_body}'"
end