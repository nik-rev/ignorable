ls __template__/ | each { |template_file| [Eq Ord Hash PartialEq Debug] | each { cp $template_file.name $"($in)/($template_file.name | path basename)" } }
