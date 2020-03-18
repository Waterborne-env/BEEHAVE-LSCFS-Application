## Read in BEEHAVE BehaviorSpace TABLE Output ##
# fp = file path to BEEHAVE output

## Function -------------------------------------------------------------------
read_beehave = function(fp){
  df = read.csv(fp, skip = 6) # Read in Raw Data
  
  ## remove columns that don't change with exceptions
  out_df = df[,colnames(df)[which(colnames(df)=='X.step.'):ncol(df)]]
  inp_df = df[,1:(which(colnames(df)=='X.step.')-1)]
  
  tmp = df[,c('INPUT_FILE', 'AssessmentFile', 'FeedingScheduleFile', 
    'WeatherFile')]
  inp_df = Filter(function(x)(length(unique(x)) > 1), inp_df)
  inp_df = cbind(tmp[!(colnames(tmp) %in% colnames(inp_df))], inp_df)
  
  df = cbind(inp_df, out_df)
  
  ## rename standard columns (if exist)
  names(df)[names(df) %in% c('INPUT_FILE', 'AssessmentFile', 
    'FeedingScheduleFile', 'WeatherFile', 'X.run.number.', 'X.step.')] = 
    c('input_file', 'assessment_file', 'feeding_schedule_file', 'weather_file',
      'run_number', 'step')
  names(df)[names(df) %in% c('totalEggs', 'totalLarvae', 'totalPupae',
    'totalIHbees', 'totalForagers', 'totalIHbees...totalForagers',
    'totalDroneEggs', 'totalDroneLarvae', 'totalDronePupae', 'totalDrones')] = 
    c('eggs', 'larvae', 'pupae', 'ihbees', 'foragers', 'adults', 'drone_eggs',
      'drone_larvae', 'drone_pupae', 'drones')
  names(df)[names(df) %in% c(
    'X.honeyEnergyStore.....ENERGY_HONEY_per_g...1000....', 'PollenStore_g')] = 
    c('honey_kg', 'pollen_g')
  
  ## clean up columns
  df$input_file = gsub('\\"|\\"', '', df$input_file)
  df$assessment_file = gsub('\\"|\\"', '', df$assessment_file)
  df$feeding_schedule_file = 
    gsub('\\"|\\"', '', df$feeding_schedule_file)
  df$weather_file = gsub('\\"|\\"', '', df$weather_file)
  
  ## add brood column
  if(all(c('eggs','larvae','pupae') %in% names(df))){
    df$brood = with(df, eggs + larvae + pupae) }
  
  return(df)
}
