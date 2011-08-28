watch('^src/(.*)\.erl') do |m|
  mod = m[1]

  if system('rebar compile skip_deps=true')
    system("rebar eunit suite=#{mod} skip_deps=true")
  end
end
