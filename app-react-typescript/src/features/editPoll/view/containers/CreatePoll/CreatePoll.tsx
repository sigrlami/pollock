import * as React from 'react';
import * as block from 'bem-cn';
import { Form, FormControl, Button, Panel, Checkbox } from 'react-bootstrap';
import { bind } from 'decko';
import { connect, Dispatch } from 'react-redux';
import { bindActionCreators } from 'redux';
import { IReduxState, IEditableQuestion } from 'shared/types/app';
import * as actions from './../../../redux/actions';
import QuestionsConstructor from './../../components/QuestionsConstructor/QuestionsConstructor';
import './CreatePoll.styl';
import FormEvent = React.FormEvent;

interface IDispatchProps {
  addAnswer: typeof actions.addAnswer;
  savePoll: typeof actions.savePoll;
  loadPoll: typeof actions.loadPoll;
  removeCurrentPoll: typeof actions.removeCurrentPoll;
  clearPollData: typeof actions.clearPollData;
  addQuestion: typeof actions.addQuestion;
  changeFieldValue: typeof actions.changeFieldValue;
  changeAnswerText: typeof actions.changeAnswerText;
  changeQuestionType: typeof actions.changeQuestionType;
  changeQuestionText: typeof actions.changeQuestionText;
}

interface IStateProps {
  questions: IEditableQuestion[];
  title: string;
  description: string;
  endTime: string;
  startTime: string;
  isActive: boolean;
}

interface IOwnProps {
  pollId?: string;
  onPollSaved?: () => void;
  onPollRemoved?: () => void;
}

function mapState(state: IReduxState): IStateProps {
  return {
    ...state.createPoll.newPoll,
  };
}

function mapDispatch(dispatch: Dispatch<any>): IDispatchProps {
  return bindActionCreators({ ...actions }, dispatch);
}

type Props = IDispatchProps & IStateProps & IOwnProps;

class CreatePoll extends React.Component<Props, {}> {
  private b = block('create-poll');

  public componentDidMount() {
    const { pollId, loadPoll } = this.props;

    if (pollId) {
      loadPoll(pollId);
    }
  }

  public componentWillUnmount() {
    // clear poll, when leave view, if it existing poll
    if (this.props.pollId) {
      this.props.clearPollData();
    }
  }

  public render() {
    const { questions, addQuestion, addAnswer, changeAnswerText } = this.props;
    const { changeQuestionText, changeQuestionType, pollId } = this.props;
    const b = this.b;

    return (
      <div className={b()}>
        <Form onSubmit={this.onSubmit}>
          <span><strong>Main information</strong></span>
          <Panel header={this.renderFormInputs()} />
          <QuestionsConstructor
            onChangeAnswerText={changeAnswerText}
            onChangeQuestionText={changeQuestionText}
            onChangeQuestionType={changeQuestionType}
            questions={questions}
            onAddQuestion={addQuestion}
            onAddAnswer={addAnswer}
          />
          <Button className={b('action')()} type="submit" bsStyle="primary">Save</Button>

          {
            pollId ? (
              <Button className={b('action')()} bsStyle="danger" onClick={this.onRemovePoll}>
                Remove
              </Button>
            ) : null
          }
        </Form>
      </div>
    );
  }

  private renderFormInputs(): React.ReactNode {
    const { title, description, endTime, startTime, isActive } = this.props;
    const b = this.b;
    return (
      <div>
        <Checkbox checked={isActive} onChange={this.onStatusChange}>
          Is active
        </Checkbox>
        <FormControl
          className={b('control')()}
          type="text"
          name="title"
          value={title}
          placeholder="Title"
          onChange={this.getOnChangeFieldHandler('title')}
          required
        />

        <FormControl
          className={b('control')()}
          componentClass="textarea"
          placeholder="Description"
          name="description"
          value={description}
          onChange={this.getOnChangeFieldHandler('description')}
          required
        />

        <FormControl
          className={b('control')()}
          type="datetime-local"
          name="startTime"
          placeholder="Format: 2014-04-24 10:00:00"
          value={startTime}
          onChange={this.getOnChangeFieldHandler('startTime')}
          required
        />
        <FormControl
          className={b('control')()}
          type="datetime-local"
          name="endTime"
          value={endTime}
          onChange={this.getOnChangeFieldHandler('endTime')}
          placeholder="Format: 2014-04-24 10:00:00"
          required
        />
      </div>
    );
  }

  @bind
  private onSubmit(e: FormEvent<Form>) {
    e.preventDefault();
    this.props.savePoll();

    if (this.props.onPollSaved) {
      this.props.onPollSaved();
    }
  }

  @bind
  private onRemovePoll(e: FormEvent<Form>) {
    e.preventDefault();
    const { removeCurrentPoll, onPollRemoved } = this.props;

    removeCurrentPoll();
    if (onPollRemoved) {
      onPollRemoved();
    }
  }

  @bind
  private onStatusChange() {
    this.props.changeFieldValue('isActive', !this.props.isActive);
  }

  private getOnChangeFieldHandler(fieldName: string): (e: FormEvent<FormControl>) => void {
    return (e: FormEvent<FormControl>) => {
      const value = (e.target as HTMLInputElement).value;
      this.props.changeFieldValue(fieldName, value);
    };
  }
}

export { CreatePoll };
export default connect<IStateProps, IDispatchProps, IOwnProps>(mapState, mapDispatch)(CreatePoll);
