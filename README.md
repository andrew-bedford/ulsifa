# ulsifa
Performs secure information-flow analyses on programs written in a simple imperative language to ensure that no sensitive information is inadvertently leaked. When necessary, it also produces an instrumented version of the code to do additional checks at runtime.

## Approach
It uses a three-valued type system to statically check non-interference. In addition to the usual two main security levels, public (or Low) and private (or High), a third security level, Unknown, is introduced to capture the possibility that we may not know whether the information is public or private statically. Standard two-valued analysis has no choice but to be pessimistic with uncertainty and hence lead to false positives. If uncertainty arises during the analysis, the instruction in cause is tagged; in a second step, instrumentation at every such point together with dynamic analysis will result in a more precise analysis than purely static approaches. This reduces the number of false positives, and introduces a light runtime overhead by instrumenting only when there is a need for it.

## Usage
The interface (_interface.jar_) is rather simple. There are two tabs: _Channels_ and _Code_. The _Channels_ tab is where you can define the communication channels that you want to use in your code (_privateChannel_ and _publicChannel_ are already defined by default). To remove one or more channels, select them and press the delete key.

<p align="center"><img src="https://i.imgur.com/Jw8S69f.png" width="66%" /></p>    

The _Code_ tab allows you to enter the code of the program that you want to analyze. To analyze the code, press the _Analyze_ button or press Ctrl+Enter. The output of the analysis is then shown on the right side of the application. If the analyzer has detected an error, whether it is a lexical, syntactic, semantic or information flow error, a message explaining the error (when possible) will be found at the end of the analyzer's output. If the code requires an instrumentation in order to be secure, then an instrumented version of the program will be generated an displayed on the right side.

<p align="center"><img src="https://i.imgur.com/5Tvv6Sj.png" width="66%" /></p>

### Examples
#### Example 1 (rejected.ulsifa)
```
x := 0;
if highValue then
    x := 1
end;
send x to publicChannel
```

#### Example 2 (rejected2.ulsifa)
```
if highValue > 42 then
    c := publicChannel
else
    c := privateChannel
end;
send lowValue to c
```

#### Example 3 (instrumented1.ulsifa)
```
if lowValue then
    c := publicChannel
else
    c := privateChannel
end;
send highValue to c
```

#### Example 4 (rejected3.ulsifa)
```
i := 0;
x1 := 0;
x2 := 0;
x3 := 0;
while i < 42 do
    send x3 to publicChannel;
    x3 := x2;
    x2 := x1;
    x1 := highValue;
    i := i + 1
end
```
This example illustrates the fact that we perform an iterative analysis when necessary. In this case, analyzing only the first iteration of the while loop would not have revealed the problem that occurs on the fourth iteration.

#### Example 5 (instrumented2.ulsifa)
```
receiven c from publicChannel;
receivec x1 from c;
x2 := 3;
receivec x3 from c;

if x1 > 2 then
    if x2 = 3 then
        send x3 to publicChannel
    end
end
```
In this example, then channel _c_ could be either private or public. This means that the security level of _x3_ is _U_ (unknown). Instead of simply rejecting this code, we instrument it so that the type of channel _c_ is checked at runtime before sending _x3_ through a public channel.
