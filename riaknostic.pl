#!/usr/bin/env perl

use strict;
use File::Slurp 'slurp';

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
    ring_creation_size => value_from_status("ring_creation_size", @status)
  );
  chomp(%riak_data);
  return %riak_data;
}

sub print_basic_data {
  my %riak_data = @_;
  say "Host operating system: $riak_data{'os'} $riak_data{'os_version'} (arch: $riak_data{'arch'})";
  say "Riak version: ", $riak_data{'riak_version'};
  say "Riak Search: ", ($riak_data{'riak_search_version'} ? "yes" : "no");
  say "Erlang runtime: $riak_data{'erlang_version'}";
  say "Number of partitions: $riak_data{'partitions'}";
  say "Ring creation size: $riak_data{'ring_creation_size'}";
}

sub find_riak {
  my($basedir) = @_;
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
    push(@errors, "Number of partitions ($riak_data{'partitions'}) doesn't equal initial ring creation size ($riak_data{'ring_creation_size'}).");
  }
}

sub find_commands {
  my @cmds = ();
  push(@cmds, `which riaksearch-admin riak-admin | tail -n 1 2>/dev/null`);
  push(@cmds, `which riaksearch riak | tail -n 1 2>/dev/null`);
  chomp(@cmds);
  return @cmds;
}

say "Running Riaknostic...";

my $basedir = $ARGV[0];
my $riak = find_riak($basedir);

my @commands = find_commands();

my($admin_cmd, $riak_cmd) = find_commands();

if (!$admin_cmd) {
  say "Couldn't find the Riak admin tool in your \$PATH";
  exit 1;
}

my $running = check_node_running($riak_cmd);

my @riak_status = `$admin_cmd status`;
my %riak_data = collect_status($riak, @riak_status);
my @errors = ();

print_basic_data(%riak_data);

say "";
say "Analyzing...";

check_ring_size_not_equals_number_partitions(%riak_data, @errors);
