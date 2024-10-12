_:

{
  programs.bottom = {
    enable = true;
  };
  xdg.configFile."bottom/bottom.toml".text = ''
    [flags]
    hide_avg_cpu = false
    dot_marker = false
    left_legend = true
    case_sensitive = false
    regex = true
    basic = false
    battery = true
    temperature_type = "c"
    color = "nord"
    [[row]]
      [[row.child]]
      ratio = 2
      type="cpu"
      [[row.child]]
      type="mem"
    [[row]]
      [[row.child]]
        type="temp"
      [[row.child]]
        type="disk"
      [[row.child]]
        type="battery"
    [[row]]
      [[row.child]]
        type="network"
      [[row.child]]
        ratio = 2
        type="process"
  '';
}
