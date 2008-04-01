file "readme.html" => "README" do
  sh 'markdown README > readme.html'
end

task :default => :selfcheck

task :clobber do
  sh 'rm -rf latest_stable/readme.html'
end

task :selfcheck do
 sh 'clj specs/spec-check_spec.clj'
end

task :samples do
  Dir["**/*_spec.clj"].each do |file|
    puts ""
    sh "clj #{file}"
  end
end

task :preprelease => [:clobber] do
  # Copy spec-check.clj to latest_stable
  sh 'cp spec-check.clj latest_stable'
end