def exec(cmd)
  sh "bundle exec #{cmd}"
end

task :default do |t|
  exec "asciidoctor -r asciidoctor-mathjax3 README.adoc"
end
