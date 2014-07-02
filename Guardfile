guard :shell do
  watch /.*\.l?hs$/ do |m|
    `runghc #{m[0]} < seeds/1`
  end
end

# vim:ft=ruby
