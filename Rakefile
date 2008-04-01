file "readme.html" => "README" do
  sh 'markdown README > readme.html'
end

task :default => ["readme.html"]

task :clobber do
  sh 'rm -rf readme.html'
end

task :check do
  # Run self-checks!
end