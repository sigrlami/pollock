import * as shortid from 'shortid';
import HttpActions from './HttpActions';
import { IEditablePoll, IPoll, IPollResult } from '../types/app';

class Api {
  private actions: HttpActions;

  constructor(public baseUrl: string, public version: string = 'v1') {
    this.actions = new HttpActions(`${baseUrl}/${version}`);
  }

  public updatePoll(poll: IEditablePoll) {
    const polls: IEditablePoll[] = this.loadRawPolls();

    // find and update
    const pollIndex = polls.findIndex(p => p.id === poll.id);
    polls[pollIndex] = poll;
    poll.questions = poll.questions.map(question => {
      return {
        ...question,
        answers: question.answers.map(answer => {
          return { ...answer, id: answer.id || shortid.generate() };
        }),
      };
    });

    localStorage.setItem('polls', JSON.stringify(polls));
  }

  public createPoll(poll: IEditablePoll) {
    const pollsData = localStorage.getItem('polls');
    const polls: IEditablePoll[] = pollsData ? JSON.parse(pollsData) : [];

    poll.id = shortid.generate();
    poll.questions = poll.questions.map(question => {
      return {
        ...question,
        answers: question.answers.map(answer => {
          return { ...answer, id: shortid.generate() };
        }),
      };
    });
    polls.push(poll);

    localStorage.setItem('polls', JSON.stringify(polls));
  }

  public loadPolls(): IPoll[] {
    const editablePolls: IEditablePoll[] = this.loadRawPolls();

    // in storage saved editable polls (need to convert to simple polls)
    return editablePolls.map<IPoll>((poll: IEditablePoll) => {
      const questions = poll.questions.map(question => {
        const answers = question.answers.map(answer => ({ ...answer, id: answer.id || '', isSelected: false }));
        return { ...question, answers };
      });

      return { ...poll, id: poll.id || shortid.generate(), questions };
    });
  }

  public loadPoll(id: string): IPoll | null {
    const polls = this.loadPolls();
    const poll = polls.find(p => p.id === id);
    return poll ? poll : null;
  }

  public removePoll(id: string) {
    this.removePollResults(id);
    const polls: IEditablePoll[] = this.loadRawPolls();
    const pollIndex = polls.findIndex(poll => poll.id === id);

    if (pollIndex > - 1) {
      polls.splice(pollIndex, 1);
    }

    localStorage.setItem('polls', JSON.stringify(polls));
  }

  public loadEditablePoll(id: string): IEditablePoll | null {
    const polls: IEditablePoll[] = this.loadRawPolls();
    const poll = polls.find(p => p.id === id);

    return poll ? poll : null;
  }

  public savePollResult(result: IPollResult) {
    const resultsData = localStorage.getItem('pollResults');
    const results: IPollResult[] = resultsData ? JSON.parse(resultsData) : [];

    result.id = shortid.generate();
    results.push(result);

    localStorage.setItem('pollResults', JSON.stringify(results));
  }

  public removePollResults(id: string) {
    const resultsData = localStorage.getItem('pollResults');
    const results: IPollResult[] = resultsData ? JSON.parse(resultsData) : [];
    const newResults = results.filter((result: IPollResult) => result.pollId !== id);
    localStorage.setItem('pollResults', JSON.stringify(newResults));
  }

  public loadPollResults(id: string): IPollResult[] {
    const resultsData = localStorage.getItem('pollResults');
    const results: IPollResult[] = resultsData ? JSON.parse(resultsData) : [];
    return results.filter((result: IPollResult) => result.pollId === id);
  }

  private loadRawPolls(): IEditablePoll[] {
    const pollsData: string | null = localStorage.getItem('polls');
    return pollsData ? JSON.parse(pollsData) : [];
  }
}

export default Api;
