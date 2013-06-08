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


## Appending an attendant

## Removing an attendant

