package Convert::Moji;

require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw/make_regex length_one unambiguous/;

use warnings;
use strict;

use Carp;

our $VERSION = '0.05';

# Load a converter from a file and return a hash reference containing
# the left/right pairs.

sub load_convertor
{
    my ($file) = @_;
    my $file_in;
    if (! open $file_in, "<:encoding(utf8)", $file) {
	carp "Could not open '$file' for reading: $!";
	return;
    }
    my %converter;
    while (my $line = <$file_in>) {
	chomp $line;
	my ($left, $right) = split /\s+/, $line;
	$converter{$left} = $right;
    }
    close $file_in or croak "Could not close '$file': $!";
    return \%converter;
}

sub length_one
{
    for (@_) {
	return if !/^.$/;
    }
    return 1;
}

sub make_regex
{
    my @inputs = @_;
    # Quote any special characters. We could also do this with join
    # '\E|\Q', but the regexes then become even longer.
    @inputs = map {quotemeta} @inputs;
    if (length_one (@inputs)) {
	return '(['.(join '', @inputs).'])';
    }
    else {
	# Sorting is essential, otherwise shorter characters match before
	# longer ones, causing errors if the shorter character is part of
	# a longer one.
	return '('.join ('|',sort { length($b) <=> length($a) } @inputs).')';
    }
}

sub unambiguous
{
    my ($table) = @_;
    my %inverted;
    for (keys %$table) {
	my $v = $$table{$_};
	return if $inverted{$v};
	$inverted{$v} = $_;
    }
    # Is not ambiguous
    return 1;
}

# If the table is unambiguous, we can use Perl's built-in "reverse"
# function. However, if the table is ambiguous, "reverse" will lose
# information. The method applied here is to make a hash with the
# values of $table as keys and the values are array references.

sub ambiguous_reverse
{
    my ($table) = @_;
    my %inverted;
    for (keys %$table) {
	my $val = $table->{$_};
	push @{$inverted{$val}}, $_;
    }
    for (keys %inverted) {
	@{$inverted{$_}} = sort @{$inverted{$_}};
    }
    return \%inverted;
}

# Callback

sub split_match
{
    my ($erter, $input, $convert_type) = @_;
    $convert_type = "first" if (!$convert_type);
    my $lhs = $erter->{rhs};
    my $rhs = $erter->{out2in};
    if (!$convert_type || $convert_type eq 'first') {
	$input =~ s/$lhs/$$rhs{$1}->[0]/eg;
	return $input;
    }
    elsif ($convert_type eq 'random') {
	my $size = @$rhs;
	$input =~ s/$lhs/$$rhs{$1}->[int rand $size]/eg;
	return $input;
    }
    elsif ($convert_type eq 'all' || $convert_type eq 'all_joined') {
	my @output = grep {length($_) > 0} (split /$lhs/, $input);
	for my $o (@output) {
	    if ($o =~ /$lhs/) {
		$o = $$rhs{$1};
	    }
	}
	if ($convert_type eq 'all') {
	    return \@output;
	}
        else {
	    return join ('',map {ref($_) eq 'ARRAY' ? "[@$_]" : $_} @output);
	}
    }
    else {
	carp "Unknown convert_type $convert_type";
    }
}

# =head2 table

# internal routine

# Attach a table to a Convert::Moji object.

#=cut

sub table
{
    my ($table, $noinvert) = @_;
    my $erter = {};
    $erter->{type} = "table";
    $erter->{in2out} = $table;
    my @keys = keys %$table;
    my @values = values %$table;
    $erter->{lhs} = make_regex @keys;
    if (!$noinvert) {
	$erter->{unambiguous} = unambiguous($table);
	if ($erter->{unambiguous}) {
	    my %out2in_table = reverse %{$table};
	    $erter->{out2in} = \%out2in_table;
	} else {
	    $erter->{out2in} = ambiguous_reverse ($table);
	    @values = keys %{$erter->{out2in}};
	}
	$erter->{rhs} = make_regex @values;
    }
    return $erter;
}

# tr_erter

# Internal routine.

# Make a converter from a tr instruction.

sub tr_erter
{
    my ($lhs, $rhs) = @_;
    my $erter = {};
    $erter->{type} = "tr";
    $erter->{lhs} = $lhs;
    $erter->{rhs} = $rhs;
    return $erter;
}

# Add a code-based converter

sub code
{
    my ($convert, $invert) = @_;
    my $erter = {};
    $erter->{type} = "code";
    $erter->{convert} = $convert;
    $erter->{invert} = $invert;
    return $erter;
}

sub new
{
    my ($package, @conversions) = @_;
    my $conv = {};
    bless $conv;
    $conv->{erter} = [];
    $conv->{erters} = 0;
    for my $c (@conversions) {
	my $noinvert;
	my $erter;
	if ($c->[0] eq "oneway") {
	    shift @$c;
	    $noinvert = 1;
	}
	if ($c->[0] eq "table") {
	    $erter = table ($c->[1], $noinvert);
	} elsif ($c->[0] eq "file") {
	    my $file = $c->[1];
	    my $table = Convert::Moji::load_convertor ($file);
	    return if !$table;
	    $erter = table ($table, $noinvert);
	} elsif ($c->[0] eq 'tr') {
	    $erter = tr_erter ($c->[1], $c->[2]);
	} elsif ($c->[0] eq 'code') {
	    $erter = code ($c->[1], $c->[2]);
	    if (!$c->[2]) {
		$noinvert = 1;
	    }
	}
	my $o = $conv->{erters};
	$conv->{erter}->[$o] = $erter;
	$conv->{noinvert}->[$o] = $noinvert;
	$conv->{erters}++;
    }
    return $conv;
}

sub convert
{
    my ($conv, $input) = @_;
    for (my $i = 0; $i < $conv->{erters}; $i++) {
	my $erter = $conv->{erter}->[$i];
	if ($erter->{type} eq "table") {
	    my $lhs = $erter->{lhs};
	    my $rhs = $erter->{in2out};
	    $input =~ s/$lhs/$$rhs{$1}/g;
	}
        elsif ($erter->{type} eq 'tr') {
	    my $lhs = $erter->{lhs};
	    my $rhs = $erter->{rhs};
	    eval ("\$input =~ tr/$lhs/$rhs/");
	}
        elsif ($erter->{type} eq 'code') {
	    $_ = $input;
	    $input = &{$erter->{convert}};
	}
    }
    return $input;
}

sub invert
{
    my ($conv, $input, $convert_type) = @_;
    for (my $i = $conv->{erters} - 1; $i >= 0; $i--) {
	next if $conv->{noinvert}->[$i];
	my $erter = $conv->{erter}->[$i];
	if ($erter->{type} eq "table") {
	    if ($erter->{unambiguous}) {
		my $lhs = $erter->{rhs};
		my $rhs = $erter->{out2in};
		$input =~ s/$lhs/$$rhs{$1}/g;
	    }
            else {
		$input = split_match ($erter, $input, $convert_type);
	    }
	}
        elsif ($erter->{type} eq 'tr') {
	    my $lhs = $erter->{rhs};
	    my $rhs = $erter->{lhs};
	    eval ("\$input =~ tr/$lhs/$rhs/");
	}
        elsif ($erter->{type} eq 'code') {
	    $_ = $input;
	    $input = &{$erter->{invert}};
	}
    }
    return $input;
}

# Split "string" using alphabet 1.

sub split_by_input_alphabet
{
    my ($conv, $string) = @_;
    my @split_string;
    my $first = $conv->{erter}->[0];
    if ($first->{type} eq 'tr') {
        my $lhs = $first->{lhs};
        @split_string = ($string =~ /([$lhs])/g);
    }
    elsif ($first->{type} eq 'table') {
        my $lhs = $first->{lhs};
        @split_string = ($string =~ /$lhs/g);
    }
    elsif ($first->{type} eq 'code') {
        croak "Can't make a regex for a code-style converter";
    }
    return @split_string;
}

# Given "string" in the input alphabet, make a regular expression in
# the output alphabet which will match any possible conversions of
# "string" into the output alphabet.

sub string2regex
{
    my ($converter, $string) = @_;
    my @string_chars = $converter->split_by_alphabet1 ($string);
    for my $c (@string_chars) {
        my @output = $converter->convert ($c);
    }

}


1;

__END__

=head1 NAME

Convert::Moji - objects to convert alphabets

=head1 SYNOPSIS

    # Examples of rot13 transformers:
    use Convert::Moji;
    # Using a table
    my %rot13;
    @rot13{('a'..'z')} = ('n'..'z','a'..'m');
    my $rot13 = Convert::Moji->new (["table", \%rot13]);
    # Using tr
    my $rot13_1 = Convert::Moji->new (["tr", "a-z", "n-za-m"]);
    # Using a callback
    sub rot_13_sub { tr/a-z/n-za-m/; return $_ }
    my $rot13_2 = Convert::Moji->new (["code", \&rot_13_sub]);

Then to do the actual conversion

    my $out = $rot13->convert ("secret");

and now $out contains "frperg". You also can go backwards with

    my $inverted = $rot13->invert ("frperg");

and now $inverted contains "secret".

=head1 DESCRIPTION

Convert::Moji creates objects which can be used to convert between
different alphabets. It was originally designed to do the work for
L<Lingua::JA::Moji>, to convert between different forms of Japanese
writing. It was split out of that module as a general-purpose
converter for any alphabets.

=head2 new

Create the object. Arguments are a list of array references. The array
references should have either the "noninvertible" flag "oneway" or one
of the following as its first argument.

You can also chain the converters
together, with

    my $does_something = Convert::Moji->new (["table", $mytable],
					     ["tr", $left, $right]);

=over

=item table

After this comes one more argument, a reference to the hash containing
the table. For example

     my $conv = Convert::Moji->new (["table", \%crazyhash]);

The hash keys and values can be any length, so you can convert single
characters into words, as in 

     my %crazyhash = {"a" => "apple", "b" => "banana"}

and vice-versa if you wish. The conversion will be performed correctly
regardless of the weirdness of your table.

=item file

After this comes one more argument, the name of a file containing some
information to convert into a hash table. The file format is
space-separated pairs, no comments or blank lines allowed. If the file
does not exist or cannot be opened, the module prints an error
message, and returns the undefined value.

=item code

After this comes one or two references to subroutines. The first
subroutine is the conversion and the second one is the inversion
routine. If you omit the second routine, it is equivalent to
specifying "oneway".

=item tr

After this come two arguments, the left and right hand sides of a "tr"
expression, for example

     Convert::Moji->new (["tr", "A-Z", "a-z"])

will convert upper to lower case

A "tr" is performed, and inversely for the invert case.

=back

Conversions, via "convert", will be performed in the order of the
arguments. Inversions will be performed in reverse order of the
arguments, skipping uninvertibles.

=head2 Uninvertible operations

If your conversion doesn't actually go backwards, you can tell the
module when you create the object using a keyword "oneway":

    my $uninvertible = Convert::Moji->new (["oneway", "table", $mytable]);

Then $uninvertible->invert doesn't do anything. You can also
selectively choose which operations of a list are invertible and which
aren't, so that only the invertible ones do something.

=head2 Load from a file

Load a character conversion table from a file using

Convert::Moji->new (["file", $filename]);

In this case, the file needs to contain a space-separated list to be
converted one into the other.

=head3 Bugs

This doesn't handle comments or blank lines in the file.

=head2 convert

The convert method takes one argument, which is a scalar string to be
converted into the other list by the stuff we fed in at "new".

=head3 Bugs

=over

=item no "strict conversion"

Just ignores (passes through) characters which it can't convert. It
should have a "strict" option to also validate the input.

=back

=cut

=head2 invert

Inverts the input.

Takes two arguments, the first is the string to be inverted back
through the conversion process, and the second is the type of conversion to perform if the inversion is ambiguous. This can take one of the following values

=over

=item first

If the inversion is ambiguous, it picks the first one it finds.

=item random

If the inversion is ambiguous, it picks one at random.

=item all

In this case you get an array reference back containing either strings
where the inversion was unambiguous, or array references to arrays
containing all possible strings. So it's a horrible mess.

=item all_joined

Like "all" but you get a scalar with all the options in square
brackets instead of lots of array references.

=back

=head3 Bugs

=over

=item second argument not implemented fully

The second argument part is only implemented for hash table based
conversions, and is very likely to be buggy even then.

=back

=head1 FUNCTIONS

These functions are used by the module and may be useful to outside
programs.

=head2 length_one

    # Returns false:
    length_one ('x', 'y', 'monkey');
    # Returns true:    
    length_one ('x', 'y', 'm');

Returns true if every element of the array has a length equal to one,
and false if any of them does not have length one.

=head2 make_regex

    my $regex = make_regex (qw/a b c de fgh/);

    # $regex = "fgh|de|a|b|c";

Given a list of inputs, make a regular expression which matches any of
the characters in the list of inputs, longest match first. Each of the
elements of the list is quoted using C<quotemeta>. The regular
expression does not contain capturing parentheses. For example, to
convert everything in string C<$x> from the keys of C<%foo2bar> to its
values,

    my %foo2bar = (mad => 'max', dangerous => 'trombone');
    my $x = 'mad, bad, and dangerous to know';
    my $regex = make_regex (keys %foo2bar);
    $x =~ s/($regex)/$foo2bar{$1}/g;
    # Now $x = "max, bad, and trombone to know".

=head2 unambiguous

    if (unambiguous (\%table)) {

    }

Returns true if all of the values in C<%table> are distinct, and false
if any two of the values in C<%table> are the same.

=head1 AUTHOR

Ben Bullock, <bkb@cpan.org>

=head1 COPYRIGHT & LICENSE

Copyright 2008-2012 Ben Bullock, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
