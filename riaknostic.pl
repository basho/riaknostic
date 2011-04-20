#!/usr/bin/env perl

use strict "vars";
use File::Slurp 'slurp';
use Cwd 'abs_path';

sub say {
  my(@to_say) = @_;
  print @to_say;
  print "\n";
}

sub value_from_status {
  my($value, @status) = @_;
  my @ret = grep(/$value/, @status);
  my $value = shift(@ret);
  $value =~ s/^.{1,} : (\<\<")?([^\>]+)("\>\>)?$/$2/;
  if ($value =~ /^\[.+\]$/) {
    $value =~ s/^\[(.+)\]$/(\1)/g;
    my @val = eval($value);
    $value = \@val;
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
  } else {
    say "Riak node is not running.";
  }
}

sub check_ring_size_not_equals_number_partitions {
  my(%riak_data, @errors) = @_;
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
 
  say join("\n", @errors)
}

say "Running Riaknostic...";

my $riak = find_riak();
my($admin_cmd, $riak_cmd) = find_commands();

if (!$admin_cmd) {
  say "Couldn't find the Riak admin tool in your \$PATH";
  exit 1;
}

my $running = check_node_running($riak_cmd);

my @riak_status = `$admin_cmd status`;
my %riak_data = collect_status($riak, `$admin_cmd status`);
$riak_data{'riak_home'} = $riak;

print_basic_data(%riak_data);
run_analysis(%riak_data);
