if [ $# -lt 1 ]
  then
    echo "An automated bash interface for calling python and R scripts involved in Simon Martin's 4 taxon D-statistic"

  else
    while getopts a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p: option
    do
    case "${option}"
    in
    a) project_name=${OPTARG};;
    b) output_directory=${OPTARG};;
    c) github_directory=${OPTARG};;
    d) simonhmartin_directory=${OPTARG};;
    e) path_to_geno_file=${OPTARG};;
    f) path_to_populations_file=${OPTARG};;
    g) path_to_admx_populations_file=${OPTARG};;

    esac
    done

Rscript ${github_directory}/ABBAwholegenome.r inputfile_${output_directory}/${project_name}.geno.tsv inputadmx_${output_directory}/${project_name}.admx.geno.tsv outputdirectory_$output_directory simonhmartin_directory_$simonhmartin_directory project_name_$project_name
