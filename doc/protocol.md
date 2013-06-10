# Jitsi-VideoBridge protocol

## Discovery

Client request Server capabilities:
```xml
<iq xmlns="jabber:client" to="xmpp.org" type="get" id="47">
  <query xmlns="http://jabber.org/protocol/disco#items" />
</iq>
```

Server returns capabilities as items, including **jitsi-videobridge** service:
```xml
<iq from='xmpp.org' to='alice@xmpp.org/jitsi' id='47' type='result'>
  <query xmlns='http://jabber.org/protocol/disco#items'>
    <item jid='conference.xmpp.org'/>
    <item jid='jitsi-videobridge.xmpp.org'/>
    <item jid='pubsub.xmpp.org'/>
    <item jid='vjud.xmpp.org'/>
</query>
</iq>
```

Client ask details about *videobridge* service:
```xml
<iq xmlns="jabber:client" to="jitsi-videobridge.xmpp.org" type="get" id="50">
  <query xmlns="http://jabber.org/protocol/disco#info" />
</iq>
```

Server returns *videobridge* identity+features:
```xml
<iq from='jitsi-videobridge.xmpp.org' to='alice@xmpp.org/jitsi' type='result' id='50'>
  <query xmlns='http://jabber.org/protocol/disco#info'>
    <identity category='component' type='conference' name='EjabberdVideoBridge'/>
    <feature var='http://jabber.org/protocol/disco#info'/>
    <feature var='http://jitsi.org/protocol/colibri'/>
  </query>
</iq>
```

## Creating new conference

Client ask server to create a videobridge:
```xml
<iq from="alice@xmpp.org/jitsi" to="jitsi-videobridge.xmpp.org" id="52" type="get">
  <conference xmlns="http://jitsi.org/protocol/colibri">
    <content name="audio">
      <channel>
        <payload-type id="96" name="opus" channels="1" clockrate="48000">
          <parameter name="usedtx" value="1"/>
        </payload-type>
        <payload-type id="8" name="PCMA" channels="1" clockrate="8000"/>
      </channel>
      <channel>
        <payload-type id="96" name="opus" channels="1" clockrate="48000">
          <parameter name="usedtx" value="1"/>
        </payload-type>
        <payload-type id="103" name="iLBC" channels="1" clockrate="8000"/>
        <payload-type id="101" name="telephone-event" channels="1" clockrate="8000"/>
      </channel>
    </content>
  </conference>
</iq>
```

Server allocate RTP and RTCP ports for each channel:
```xml
<iq type="result" id="52" from="jitsi-videobridge.xmpp.org" to="alice@xmpp.org/jitsi">
  <conference xmlns="http://jitsi.org/protocol/colibri" id="82671f14104256bb">
    <content name="audio">
      <channel id="f24bb8fd74e5ee83" host="275.32.1.47" rtpport="10014" rtcpport="10015" expire="60"/>
      <channel id="a453c450091b3325" host="275.32.1.47" rtpport="17245" rtcpport="17246" expire="60"/>
    </content>
  </conference>
</iq>
```

## Appending an attendant

## Removing an attendant

```xml
<iq type="set" id="123" to="jitsi-videobridge.xmpp.org" from="alice@xmpp.org/jitsi">
  <conference xmlns="http://jitsi.org/protocol/colibri" id="82671f14104256bb">
    <content name="audio">
      <channel id="f24bb8fd74e5ee83" expire="0"/>
    </content>
  </conference>
</iq>
```

```xml
<iq type="result" id="123" from="jitsi-videobridge.bour.cc" to="alice@xmpp.org/jitsi">
  <conference xmlns="http://jitsi.org/protocol/colibri" id="82671f14104256bb">
    <content name="audio"/>
  </conference>
</iq>
```
