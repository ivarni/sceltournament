import Duel from 'duel';
import uuid from 'uuid';

let tournament;

const app = Elm.Main.init({
    node: document.getElementById('elm-root')
});

app.ports.create.subscribe(data => {
    tournament = new Duel(data);
    /*
     * The generated matches have their own ids but they
     * consist of 3 values
     *  s: Number, // the bracket - either WB (1) or LB (2)
     *  r: Number, // the round number in the current bracket
     *  m: Number  // the match number in the current bracket and round
     *
     * Elm code currently doesn't care about winner/looser brackets,
     * rounds or matches so generate a uuid for each match instead
     */
    tournament.matches.forEach(m => {
        m.elmId = uuid.v4();
    });

    sendUpdate();
});

app.ports.score.subscribe(({ winnerId, matchId }) => {
    const match = tournament.matches.find(
        m => m.elmId == matchId
    );
    /*
     * Scoring expects the actual score but just assume
     * all matches end 1-0 or 0-1
     */
    const result = match.p[0] == winnerId ? [1, 0] : [0, 1];

    tournament.score(match.id, result);

    sendUpdate();
});

const sendUpdate = () =>
    app.ports.onMatchupsUpdated.send(
        tournament.matches
            /*
             * Filtering out the loosers bracket matches
             * (m.id.s == 1)
             * to keep it simple
             */
            .filter(m => m.id.s == 1)
            /*
             * Filtering out matches with walkovers
             * (some p id of -1)
             * Should probably show these eventually
             */
            .filter(m => m.p.every(p => p != -1))
            .map(mapToElmModel)
    )

const mapToElmModel = m => ({
    id: m.elmId,
    /*
     * id 0 (in "p" array) is used to indicate no player but use null instead
     */
    playerOne: m.p[0] || null,
    playerTwo: m.p[1] || null,
    round: m.id.r,
    winner: m.m ?
        m.m[0] > m.m[1] ? m.p[0] : m.p[1] :
        null
});
