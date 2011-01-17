
# Query goobook
contacts = []

`goobook query '' | awk -F"\t" 'NR==1{next}{print $2 , "<"$1">"}'`.each do |contact|
  contacts.push contact
end

contacts