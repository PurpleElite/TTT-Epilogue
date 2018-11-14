import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

let app = Elm.Main.init({
  node: document.getElementById('root')
});

let soundsPlaying = [];

app.ports.portOut.subscribe( ( message ) => {
    let messageType = message.Constructor;
    switch (messageType)
    {
        case "PlaySound":
        {
            let messageBody = message.A1;
            var sound = new Audio("../" + messageBody.soundName);
            sound.currentTime = 0;
            sound.loop = messageBody.loop;
            sound.play();
            soundsPlaying.push({ soundName: messageBody.soundName, sound: sound });
            break;
        }
        case "StopSound":
        {
            let messageBody = message.A1;
            for (let i = soundsPlaying.length - 1; i >= 0; i--) {
                if (soundsPlaying[i].soundName === messageBody.soundName) {
                    soundsPlaying[i].sound.pause();
                }
            }
            break;
        }
        default:
            console.log("Message type \"" + messageType + "\" was not handled.");
            debugger;
            break;
    }
});

registerServiceWorker();
