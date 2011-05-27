#!/usr/bin/env perl

use strict "vars";
use File::Slurp 'slurp';
use Cwd 'abs_path';

sub say {
  my(@to_say) = @_;
  print @to_say;
  print "\n";
}

sub align_status_output { 
  my(@output) = @_; 
  my $full_line = ""; 
  my @result = (); 
  foreach my $line (@output) { 
    chomp($line); 
    if ($line =~ /[a-z_] :/) { 
      if ($full_line) { 
        $full_line .= "\n"; 
        push(@result, $full_line); 
      } 
      $full_line = $line; 
    } else { 
      $line =~ s/^\s+//; 
      $full_line .= $line; 
    } 
  } 
  @result; 
} 
                                                         
sub value_from_status {
  my($value, @status) = @_;
  my @ret = grep(/$value/, @status);
  my $value = shift(@ret);
  my @val;
  $value =~ s/^.{1,} : (\<\<"|')?([^\>]+)("\>\>|')?$/$2/;
  if ($value =~ /^\[.+\]$/) {
    $value =~ s/^\[(.+)\]$/(\1)/g;
    @val = eval($value);
    $value = \@val;
  }
  
  if ($value =~ /'$/) {
    $value =~ s/(.+)'$/\1/;
  }
  $value;
}

sub collect_status {
  my($riak, @status) = @_;
  my %riak_data = (
    riak_version => pop(@{[split(" ", slurp("$riak/start_erl.data"))]}),
    arch => `uname -p`,
    os => `uname -s`,
    os_version => `uname -r`,
    riak_search_version => value_from_status("riak_search_core_version", @status),
    erlang_version => value_from_status("sys_system_version", @status),
    partitions => value_from_status("ring_num_partitions", @status),
    ring_creation_size => value_from_status("ring_creation_size", @status),
    nodes_count => value_from_status("ring_members", @status),
    nodename => value_from_status("nodename", @status),
    connected_nodes => value_from_status("connected_nodes", @status),
    logs => guess_log_directory($riak)
  );
  chomp(%riak_data);
  %riak_data;
}

sub print_basic_data {
  my %riak_data = @_;
  say "Host operating system: $riak_data{'os'} $riak_data{'os_version'} (arch: $riak_data{'arch'})";
  say "Riak version: ", $riak_data{'riak_version'};
  say "Riak Search: ", ($riak_data{'riak_search_version'} ? "yes" : "no");
  say "Erlang runtime: $riak_data{'erlang_version'}";
  say "Number of partitions: $riak_data{'partitions'}";
  say "Ring creation size: $riak_data{'ring_creation_size'}";
  say "Number of nodes: ", scalar(@{$riak_data{'nodes_count'}});
  say "Number of connected nodes: ", scalar(@{$riak_data{'connected_nodes'}});
}

sub find_riak {
  my $basedir = $ARGV[0];
  my @release_dirs = ("$basedir/libexec/releases", "$basedir/releases", "$basedir");
  my $dir;
  foreach $dir (@release_dirs) {
    if (-e "$dir/start_erl.data") {
      say "Riak node found in $dir";
      return $dir
    }
  }
  say "Couldn't find a Riak installation in $basedir";
  exit 1;
}

sub check_node_running {
  my($riak) = @_;
  my @output = `$riak ping`;
  if (grep(/pong/, @output)) {
    say "Riak node is running.";
    1;
  } else {
    say "Riak node is not running.";
    0;
  }
}

sub check_ring_size_not_equals_number_partitions {
  my(%riak_data, @errors) = @_;
  return unless $riak_data{'running'};
  if ($riak_data{'partitions'} != $riak_data{'ring_creation_size'}) {
    push(@{${[1]}}, "Number of partitions ($riak_data{'partitions'}) doesn't equal initial ring creation size ($riak_data{'ring_creation_size'}).");
  }
}

sub check_dump_files {
  my(%riak_data) = %{$_[0]};
  my $logs = $riak_data{'logs'};
  if (-e "$logs/erl_crash.dump") {
    push(@{$_[1]}, "Found an Erlang crash dump in $logs");
  }
}

sub check_emfile_errors {
  my(%riak_data) = %{$_[0]};
  my $logs = $riak_data{'logs'};
  my @erlang_logs = <$logs/erlang.log.*>;
  for my $log (@erlang_logs) {
    open FILE, "<$log";
    if (grep /emfile/, <FILE>) {
      push(@{$_[1]}, "Found errors indicating there are not enough available file descriptors. Increase the value using ulimit -n");
    }
  }
}

sub check_number_of_partitions_and_nodes {
  my(%riak_data) = %{$_[0]};
  return unless $riak_data{'running'};
  my $partitions = $riak_data{'partitions'};
  my $nodes = scalar(@{$riak_data{'nodes_count'}});
  my $ratio = $partitions / $nodes;
  if ($ratio < 8 or $ratio > 64) {
    push(@{$_[1]}, "The number of partitions per node ($ratio) is less than recommended. Should be between 8 and 64.")
  }
}

sub check_node_part_of_ring {
  my(%riak_data) = %{$_[0]};
  my $node = $riak_data{'nodename'};
  my @ring_members = @{$riak_data{'nodes_count'}};
  if (not grep {$_ eq $node} @ring_members) {
    push(@{$_[1]}, "The current node is not part of the Riak ring.")
  }
}

sub check_connected_nodes {
  my(%riak_data) = %{$_[0]};
  return unless $riak_data{'running'};
  my $my_node = $riak_data{'nodename'};
  my @ring_members = @{$riak_data{'nodes_count'}};
  my @connected_nodes = @{$riak_data{'connected_nodes'}};
  for my $node (@ring_members) {
    next if $node eq $my_node;
    if (not grep {$_ eq $node} @connected_nodes) {
      push(@{$_[1]}, "Node $node is part of the ring but not connected.")
    }
  }
}

sub find_commands {
  my @cmds = ();
  push(@cmds, `which riaksearch-admin riak-admin | tail -n 1 2>/dev/null`);
  push(@cmds, `which riaksearch riak | tail -n 1 2>/dev/null`);
  chomp(@cmds);
  return @cmds;
}

sub guess_log_directory {
  my($riak) = @_;
  my @log_directories = ("$riak/../log", "$riak/log", "/var/log/riak", "/opt/riak/log");
  for my $dir (@log_directories) {
    if (-d $dir) {
      $dir = abs_path($dir);
      say "Using Riak logs in $dir";
      return $dir;
    }
  }
}

sub run_analysis {
  my %riak_data = @_;
  say "\nAnalyzing...";
  my @errors = ();

  &check_ring_size_not_equals_number_partitions(\%riak_data, \@errors);
  &check_dump_files(\%riak_data, \@errors);
  &check_emfile_errors(\%riak_data, \@errors);
  &check_number_of_partitions_and_nodes(\%riak_data, \@errors);
  &check_node_part_of_ring(\%riak_data, \@errors);
  &check_connected_nodes(\%riak_data, \@errors);
 
  if (scalar(@errors) > 0) {
    say join("\n", @errors);
  } else {
    say "All good!";
  }
  say "";
}

say "Running Riaknostic...";

my $riak = find_riak();
my($admin_cmd, $riak_cmd) = find_commands();

if (!$admin_cmd) {
  say "Couldn't find the Riak admin tool in your \$PATH";
  exit 1;
}

my $running = check_node_running($riak_cmd);

my @riak_status = align_status_output(`$admin_cmd status`);
my %riak_data = collect_status($riak, @riak_status);
$riak_data{'riak_home'} = $riak;
$riak_data{'running'} = $running;

print_basic_data(%riak_data);
run_analysis(%riak_data);
