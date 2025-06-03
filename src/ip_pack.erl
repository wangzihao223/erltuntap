-module(ip_pack).

-define(DEFAULT, 0).
-define(EF, 46).
-define(AF11, 10).
-define(AF21, 10).
-define(AF31, 18).
-define(TTL, 64).

-export([init_config/0, make_ip_head/1, make_id/1]).

-record(ip_head,
        {version = 4,
         ihl = 0,
         dscp = ?DEFAULT,
         ecn = 2,
         total_length = 0,
         identification = nil,
         flags = nil,
         fragment_offset = nil,
         time_to_live = nil,
         protocol = nil,
         header_checksum = nil,
         source_addr = nil,
         destination_addr = nil,
         option = nil,
         padding = nil}).

%%
%%
%%September 1981
%Internet Protocol
%3. SPECIFICATION
%3.1. Internet Header Format
%A summary of the contents of the internet header follows:
%0 1 2 3
%0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%|Version| IHL |Type of Service| Total Length |
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%| Identification |Flags| Fragment Offset |
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%| Time to Live | Protocol | Header Checksum |
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%| Source Address |
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%| Destination Address |
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%| Options | Padding |
%+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

init_config() ->
  Config =
    #{service => best_effort,
      total_length => 0,
      is_support_ecn => true,
      is_congest => false,
      fragment_flag => false,
      has_fragment => false,
      fragment_offset => nil,
      protocol => tcp,
      src_ip => ip_to_int("192.168.0.1"),
      dst_ip => ip_to_int("8.8.8.8"),
      option => nil,
      id => nil},
  Config.

make_ip_head(Config) ->
  Service = map_get(Config, service),
  SupportECN = map_get(Config, is_support_ecn),
  IsCongest = map_get(Config, is_congest),
  FragmentFlag = map_get(Config, fragment_flag),
  HasFragment = map_get(Config, has_fragment),
  FragmentOffset = map_get(Config, fragment_offset),
  Protocol = map_get(Config, protocol),
  SrcIP = map_get(Config, src_ip),
  DstIP = map_get(Config, dst_ip),
  _Option = map_get(Config, option),
  TL = map_get(Config, total_length),
  ID = map_get(Config, id),

  Dscp = service_to_dscp(Service),
  Ecn = make_ecn(SupportECN, IsCongest),
  TOS = make_type_of_service(Dscp, Ecn),
  F = get_flag(FragmentFlag, HasFragment),
  Flags = make_flags_fragment_offset(F, FragmentOffset),
  T2L = ?TTL,
  ProtocolNumber = protocol_number(Protocol),
  HeaderChecksum = 0,

  IHL = 20,
  Part1 = <<4:4, IHL:4, TOS:8, TL:16, ID:16, Flags:16>>,
  Part2 = <<T2L:8, ProtocolNumber:8, HeaderChecksum:16>>,
  Part3 = <<SrcIP:32, DstIP:32>>,
  HeaderRaw = <<Part1/binary, Part2/binary, Part3/binary>>,
  CheckSum = ip_header_checksum(HeaderRaw),

  Part4 = <<T2L:8, ProtocolNumber:8, CheckSum:16>>,
  Head = <<Part1/binary, Part4/binary, Part3/binary>>,
  HeadAttr =
    #ip_head{ihl = IHL,
             dscp = Dscp,
             ecn = Ecn,
             total_length = TL,
             identification = ID,
             flags = F,
             fragment_offset = FragmentOffset,
             time_to_live = T2L,
             protocol = ProtocolNumber,
             header_checksum = CheckSum,
             source_addr = SrcIP,
             destination_addr = DstIP},

  {Head, HeadAttr}.

make_ecn(SupportFlag, IsCongest) ->
  case {SupportFlag, IsCongest} of
    {false, _} ->
      0;       % 不支持 ECN，返回 Not-ECT (0)
    {true, true} ->
      3;    % 支持 ECN 且拥塞，返回 CE (3)
    {true, false} ->
      1    % 支持 ECN 且不拥塞，返回 ECT(1) (1)
  end.

% 根据用途选择dscp
service_to_dscp(best_effort) ->
  0;  % 默认
service_to_dscp(expedited_forwarding) ->
  46;  % EF PHB
service_to_dscp(assured_forwarding) ->
  10;   % AF11 示例
service_to_dscp(_Other) ->
  0.

make_type_of_service(Dscp, Ecn) ->
  Dscp bsl 2 bor Ecn.

make_id(Pid) ->
  Pid ! {get_id, self()},
  receive
    {id, ID} ->
      ID
  end.

%% 构造 Flags|FragmentOffset 字段
%% Flags是一个3位的整数，只有最低3位有效，FragmentOffset是13位
make_flags_fragment_offset(Flags, FragmentOffset) ->
  %% 参数校验
  FlagsMasked = Flags band 16#7,                % 3 bits
  FragmentMasked = FragmentOffset band 16#1FFF, % 13 bits
  FlagsMasked bsl 13 bor FragmentMasked.

get_flag(df, nil) ->
  2;
get_flag(mf, true) ->
  1;
get_flag(mf, false) ->
  0;
get_flag(_, _) ->
  erlang:error({bad_flag_argument, "invalid flag or value"}).

protocol_number(icmp) ->
  1;
protocol_number(tcp) ->
  6;
protocol_number(udp) ->
  17;
protocol_number(gre) ->
  47;
protocol_number(esp) ->
  50;
protocol_number(ah) ->
  51;
protocol_number(ospf) ->
  89;
protocol_number(sctp) ->
  132;
protocol_number(_) ->
  erlang:error(bad_protocol).

-spec ip_header_checksum(binary()) -> integer().
ip_header_checksum(Header) ->
  %% 校验和字段（第11、12字节）应置0后计算
  ZeroedHeader = zero_checksum_field(Header),
  Words = binary_to_words(ZeroedHeader),
  Sum = sum_words(Words),
  Checksum = ones_complement(Sum),
  Checksum.

%% 将校验和字段置为0（第11、12字节，偏移10-11）
-spec zero_checksum_field(binary()) -> binary().
zero_checksum_field(Header) ->
  <<Before:80/bits, _Checksum:16, After/binary>> = Header,
  <<Before:80/bits, 0:16, After/binary>>.

%% 把二进制按16位分割成整数列表
-spec binary_to_words(binary()) -> [integer()].
binary_to_words(Bin) ->
  binary_to_words(Bin, []).

binary_to_words(<<>>, Acc) ->
  lists:reverse(Acc);
binary_to_words(<<Word:16, Rest/binary>>, Acc) ->
  binary_to_words(Rest, [Word | Acc]).

%% 对所有16位字求和，若超过16位要进位相加（模拟16位溢出）
-spec sum_words([integer()]) -> integer().
sum_words(Words) ->
  Sum = lists:foldl(fun(X, Acc) -> Acc + X end, 0, Words),
  fold_carry(Sum).

fold_carry(Sum) when Sum > 16#FFFF ->
  %% 把高16位加回低16位（进位折叠）
  fold_carry(Sum band 16#FFFF + (Sum bsr 16));
fold_carry(Sum) ->
  Sum.

%% 求1的补码：按位取反（16位）
-spec ones_complement(integer()) -> integer().
ones_complement(Value) ->
  bnot Value band 16#FFFF.

ip_to_int(IpStr) ->
  {ok, {A, B, C, D}} = inet:parse_address(IpStr),
  A bsl 24 bor (B bsl 16) bor (C bsl 8) bor D.
