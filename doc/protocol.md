# Jitsi-VideoBridge protocol

## Discovery

Client request Server capabilities:
``̀`xml
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
``̀ 

Client as details about *videobridge* service:
```xml
<iq xmlns="jabber:client" to="jitsi-videobridge.bour.cc" type="get" id="50">
  <query xmlns="http://jabber.org/protocol/disco#info" />
</iq>
```

Server returns *videobridge* identity+features:
``̀ xml
<iq from='jitsi-videobridge.bour.cc' to='gajim2@bour.cc/Gajim' type='result' id='50'>
  <query xmlns='http://jabber.org/protocol/disco#info'>
    <identity category='component' type='conference' name='EjabberdVideoBridge'/>
    <feature var='http://jabber.org/protocol/disco#info'/>
    <feature var='http://jitsi.org/protocol/colibri'/>
  </query>
</iq>
```

## Creating new conference

## Appending an attendant

## Removing an attendant

