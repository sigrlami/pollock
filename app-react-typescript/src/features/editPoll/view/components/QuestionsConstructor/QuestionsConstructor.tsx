import * as React from 'react';
import * as block from 'bem-cn';
import { Button, Glyphicon, FormControl } from 'react-bootstrap';
import { bind } from 'decko';
import { IEditableQuestion, QuestionType } from 'shared/types/app';
import { Option } from 'shared/view/elements/SelectInput/SelectInput';
import Question from './QuestionConstructor';
import FormEvent = React.FormEvent;

interface IProps {
  questions: IEditableQuestion[];
  onAddQuestion?: () => void;
  onAddAnswer?: (questionIndex: number) => void;
  onChangeAnswerText?: (questionIndex: number, answerIndex: number, value: string) => void;
  onChangeQuestionType?: (questionIndex: number, value: QuestionType) => void;
  onChangeQuestionText?: (questionIndex: number, value: string) => void;
}

class QuestionsConstructor extends React.Component<IProps, {}> {
  public static questionsTypesOptions = [
    { label: 'YesNo', value: 'YesNo' },
    { label: 'SingleChoice', value: 'SingleChoice' },
  ];
  private b = block('questions-constructor');

  public render() {
    const b = this.b;
    const { questions } = this.props;

    return (
      <div className={b()}>
        <p><strong>Add questions</strong></p>

        <div className={b('questions')}>
          {
            questions.map((question: IEditableQuestion, index: number) => (
              <Question
                key={index}
                type={question.type}
                text={question.text}
                answers={question.answers}
                types={QuestionsConstructor.questionsTypesOptions}
                onTextChange={this.getOnQuestionTextChange(index)}
                onTypeChange={this.getOnQuestionTypeChange(index)}
                onAnswerTextChange={this.getOnAnswerTextChange(index)}
                onAddAnswerClick={this.getOnAddAnswerHandler(index)}
              />
            ))
          }
        </div>

        <Button block onClick={this.onAddQuestion}>
          <Glyphicon glyph="plus" />
        </Button>
      </div>
    );
  }

  @bind
  private onAddQuestion() {
    const handler = this.props.onAddQuestion;

    if (handler) {
      handler();
    }
  }

  @bind
  private getOnAddAnswerHandler(questionIndex) {
    return () => {
      const handler = this.props.onAddAnswer;

      if (handler) {
        handler(questionIndex);
      }
    };
  }

  @bind
  private getOnAnswerTextChange(questionIndex: number) {
    return (answerIndex: number, e: FormEvent<FormControl>) => {
      const value = (e.target as HTMLInputElement).value;
      const handler = this.props.onChangeAnswerText;
      if (handler) {
        handler(questionIndex, answerIndex, value);
      }
    };
  }

  @bind
  private getOnQuestionTextChange(questionIndex: number) {
    return (e: FormEvent<FormControl>) => {
      const value = (e.target as HTMLInputElement).value;
      const handler = this.props.onChangeQuestionText;
      if (handler) {
        handler(questionIndex, value);
      }
    };
  }

  @bind
  private getOnQuestionTypeChange(questionIndex: number) {
    return (type: Option) => {
      const value = type.value;
      const handler = this.props.onChangeQuestionType;
      if (handler) {
        handler(questionIndex, value as QuestionType);
      }
    };
  }
}

export default QuestionsConstructor;
